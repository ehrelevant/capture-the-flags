from __future__ import annotations
from dataclasses import dataclass, field
import json
from cs150241project_networking import CS150241ProjectNetworking, Message
from model import BoardGameModel
from view import BoardGameView, GameStateObserver
from project_types import (
    Location,
    GameState,
    LocationDict,
    MakeMoveGameMessageContentDict,
    PlacePieceGameMessageContentDict,
    Player,
    PieceKind,
    MoveFeedback,
    PlaceFeedback,
    GameMessageType,
    GameMessageDict,
    GameMessageContentDict,
)

# --- MARK: BoardGameControler


class BoardGameController:
    _model: BoardGameModel
    _view: BoardGameView
    _networking: CS150241ProjectNetworking | None
    _game_state_observers: list[GameStateObserver]

    def __init__(
        self,
        model: BoardGameModel,
        view: BoardGameView,
        networking: CS150241ProjectNetworking | None,
    ) -> None:
        self._model = model
        self._view = view
        self._networking = networking
        self._game_state_observers = []

    def start(self) -> None:
        view: BoardGameView = self._view
        self._game_state_observers.append(view)
        view.register_move_piece_observer(self)
        view.register_place_piece_observer(self)
        view.register_receive_message_observer(self)

        view.run(self._networking)

    def on_move_piece(self, old: Location, new: Location, player: Player) -> None:
        # check if move is valid
        is_move_valid: bool = self._model.is_move_valid(old, new, player)

        feedback: MoveFeedback = self._model.move_piece(old, new, player)
        self._view.update_move(feedback)
        self._on_state_change(self._model)
        

        # ---

        # broadcast to other players if message originated from client
        if (
            self._networking is not None
            and player == self._model.player
            and is_move_valid
        ):
            message_content: MakeMoveGameMessageContentDict = {
                'move_src': {'row': old.row, 'col': old.column},
                'move_dest': {'row': new.row, 'col': new.column},
            }

            data: GameMessageDict = {
                'message_type': GameMessageType.MOVE,
                'message_content': message_content,
            }

            self._networking.send(json.dumps(data))

    def on_place_piece(
        self, piece_kind: PieceKind, dest: Location, player: Player
    ) -> None:
        # check if place is valid
        is_place_valid: bool = self._model.is_place_valid(piece_kind, dest, player)

        feedback: PlaceFeedback = self._model.place_piece(piece_kind, dest, player)
        self._view.update_place(feedback)
        self._on_state_change(self._model)
        

        # ---

        # broadcast to other players if message originated from client
        if (
            self._networking is not None
            and player == self._model.player
            and is_place_valid
        ):
            message_content: PlacePieceGameMessageContentDict = {
                'place_piece_kind': piece_kind,
                'place_dest': {'row': dest.row, 'col': dest.column},
            }

            data: GameMessageDict = {
                'message_type': GameMessageType.PLACE,
                'message_content': message_content,
            }
            self._networking.send(json.dumps(data))

    def on_receive_message(self, message: Message) -> None:
        if self._networking is not None:
            if self._networking.player_id != message.source:
                # print(f'Message from Player {message.source}: {message.payload}')
                game_message: GameMessage = GameMessageFactory.make(message.payload)

                message_type: GameMessageType = game_message.message_type
                message_content: GameMessageContent = game_message.message_content

                match GameMessageType(message_type):
                    case GameMessageType.MOVE:
                        if message_content.move_src is None:
                            return

                        if message_content.move_dest is None:
                            return

                        match message.source:
                            case 1:
                                player: Player = Player.PLAYER_1
                            case 2:
                                player = Player.PLAYER_2
                            case _:
                                return

                        self.on_move_piece(
                            message_content.move_src,
                            message_content.move_dest,
                            player,
                        )
                    case GameMessageType.PLACE:
                        if message_content.place_piece_kind is None:
                            return

                        if message_content.place_dest is None:
                            return

                        match message.source:
                            case 1:
                                player: Player = Player.PLAYER_1
                            case 2:
                                player = Player.PLAYER_2
                            case _:
                                return

                        self.on_place_piece(
                            message_content.place_piece_kind,
                            message_content.place_dest,
                            player,
                        )
                    case GameMessageType.INVALID:
                        return

    def _on_state_change(self, state: GameState) -> None:
        for observer in self._game_state_observers:
            observer.on_state_change(state)


# --- MARK: GameMessage


@dataclass(frozen=True)
class GameMessageContent:
    _dict: GameMessageContentDict = field(default_factory=lambda: {})

    @classmethod
    def default_invalid(cls) -> GameMessageContent:
        return cls()

    @property
    def player(self) -> Player | None:
        return self._dict.get('player', None)

    @property
    def move_src(self) -> Location | None:
        value: LocationDict | None = self._dict.get('move_src', None)

        if value:
            return Location(value['row'], value['col'])

    @property
    def move_dest(self) -> Location | None:
        value: LocationDict | None = self._dict.get('move_dest', None)

        if value:
            return Location(value['row'], value['col'])

    @property
    def place_piece_kind(self) -> PieceKind | None:
        return self._dict.get('place_piece_kind', None)

    @property
    def place_dest(self) -> Location | None:
        value: LocationDict | None = self._dict.get('place_dest', None)

        if value:
            return Location(value['row'], value['col'])


class GameMessage:
    _message_type: GameMessageType
    _message_content: GameMessageContent

    def __init__(
        self,
        message_type: GameMessageType,
        message_content: GameMessageContent,
    ) -> None:
        self._message_type = message_type
        self._message_content = message_content

    @property
    def message_type(self) -> GameMessageType:
        return self._message_type

    @property
    def message_content(self) -> GameMessageContent:
        return self._message_content


class GameMessageFactory:
    @classmethod
    def make(cls, data_raw: str) -> GameMessage:
        # ensure that data is a valid GameMessageDict; no way to actually enforce in Python other than this
        try:
            data: GameMessageDict = json.loads(data_raw)
        except json.JSONDecodeError:
            return GameMessage(GameMessageType.INVALID, GameMessageContent())
        except Exception:
            return GameMessage(GameMessageType.INVALID, GameMessageContent())

        try:
            # extract keys from GameMessage
            # note: has a chance for uncaught errors if data doesn't throw an exception but is not of type GameMessageDict
            if 'message_type' not in data or 'message_content' not in data:
                message_type: GameMessageType = GameMessageType.INVALID
                message_content: GameMessageContentDict = {}
            else:
                message_type = data['message_type']
                message_content = data['message_content']

            # make message content based on message type
            match message_type:
                case GameMessageType.INVALID:
                    return GameMessage(GameMessageType.INVALID, GameMessageContent())
                case _:
                    return GameMessage(
                        message_type,
                        GameMessageContent(message_content),
                    )
        except Exception:
            return GameMessage(GameMessageType.INVALID, GameMessageContent())
