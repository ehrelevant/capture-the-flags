from __future__ import annotations
from cs150241project_networking.main import PlayerId
import copy
from dataclasses import dataclass, field, replace
from typing import Protocol

from project_types import (
    GameStatus,
    Player,
    PieceKind,
    Location,
    MoveFeedback,
    MoveFeedbackInfo,
    PlaceFeedback,
    PlaceFeedbackInfo,
)

# --- MARK: Movement


class Movement(Protocol):
    def get_deltas(self) -> set[Location]: ...


# Pawn
class PawnMovement:
    _player: Player

    def __init__(self, player: Player):
        self._player = player

    @property
    def _forward(self) -> int:
        match self._player:
            case Player.PLAYER_1:
                return 1
            case Player.PLAYER_2:
                return -1

    def get_deltas(self) -> set[Location]:
        return {Location(self._forward, 0)}


# Grail
class GrailMovement:
    def get_deltas(self) -> set[Location]:
        return {
            Location(dr, dc)
            for dr in [-1, 0, 1]
            for dc in [-1, 0, 1]
            if dr != 0 or dc != 0
        }


# Lance
class LanceMovement:
    def get_deltas(self) -> set[Location]:
        return {Location(dr, dc) for dr in [-1, 1] for dc in [-1, 1]}


# Flag Left/Right
class FlagLeftMovement:
    _player: Player

    def __init__(self, player: Player):
        self._player = player

    @property
    def _abs_left(self) -> int:
        return -1

    def get_deltas(self) -> set[Location]:
        return {
            Location(dr, dc)
            for (dr, dc) in [
                (-1, 0),
                (1, 0),
                (-1, self._abs_left),
                (0, self._abs_left),
                (1, self._abs_left),
            ]
        }


class FlagRightMovement:
    _player: Player

    def __init__(self, player: Player):
        self._player = player

    @property
    def _abs_right(self) -> int:
        return 1

    def get_deltas(self) -> set[Location]:
        return {
            Location(dr, dc)
            for (dr, dc) in [
                (1, 0),
                (-1, 0),
                (-1, self._abs_right),
                (0, self._abs_right),
                (1, self._abs_right),
            ]
        }


class SwordMovement:
    _player: Player

    def __init__(self, player: Player):
        self._player = player

    @property
    def _forward(self) -> int:
        match self._player:
            case Player.PLAYER_1:
                return 1
            case Player.PLAYER_2:
                return -1

    def get_deltas(self) -> set[Location]:
        return {Location(self._forward, dc) for dc in [-1, 0, 1]}


class BowMovement:
    _player: Player

    def __init__(self, player: Player):
        self._player = player

    @property
    def _forward(self) -> int:
        match self._player:
            case Player.PLAYER_1:
                return 1
            case Player.PLAYER_2:
                return -1

    @property
    def _backward(self) -> int:
        match self._player:
            case Player.PLAYER_1:
                return -1
            case Player.PLAYER_2:
                return 1

    def get_deltas(self) -> set[Location]:
        return {Location(self._forward, dc) for dc in [-1, 0, 1]}.union(
            {
                Location(dr, dc)
                for (dr, dc) in [(2 * self._forward, 0), (self._backward, 0)]
            }
        )


class DaggerMovement:
    _player: Player

    def __init__(self, player: Player):
        self._player = player

    @property
    def _backward(self) -> int:
        match self._player:
            case Player.PLAYER_1:
                return -1
            case Player.PLAYER_2:
                return 1

    def get_deltas(self) -> set[Location]:
        return {
            Location(dr, dc)
            for (dr, dc) in [(0, -1), (0, 1), (-1, 0), (1, 0), (2 * self._backward, 0)]
        }


# --- MARK: Piece


class Piece(Protocol):
    _player: Player
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool
    _can_capture: bool

    @property
    def player(self) -> Player: ...

    @property
    def piece_kind(self) -> PieceKind: ...

    @property
    def location(self) -> Location: ...

    @property
    def is_protected(self) -> bool: ...

    @property
    def can_capture(self) -> bool: ...

    @property
    def destinations(self) -> set[Location]: ...

    def can_move(self, dest: Location) -> bool: ...

    def move(self, dest: Location): ...


@dataclass
class RegularPiece:
    _player: Player
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool = field(init=False, default=False)
    _can_capture: bool = field(init=False, default=True)

    @property
    def player(self) -> Player:
        return self._player

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def location(self) -> Location:
        return self._location

    @property
    def is_protected(self) -> bool:
        return self._is_protected

    @property
    def can_capture(self) -> bool:
        return self._can_capture

    @property
    def destinations(self) -> set[Location]:
        return {self._location + delta for delta in self._movement.get_deltas()}

    def can_move(self, dest: Location) -> bool:
        return dest - self._location in self._movement.get_deltas()

    def move(self, dest: Location) -> None:
        if not self.can_move(dest):
            raise ValueError(f'Error: RegularPiece cannot move to square {dest}')

        self._location = copy.copy(dest)


@dataclass
class ProtectedPiece:
    _player: Player
    _piece_kind: PieceKind
    _location: Location
    _movement: Movement
    _is_protected: bool = field(init=False, default=True)
    _can_capture: bool = field(init=False, default=False)

    @property
    def player(self) -> Player:
        return self._player

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def location(self) -> Location:
        return self._location

    @property
    def is_protected(self) -> bool:
        return self._is_protected

    @property
    def can_capture(self) -> bool:
        return self._can_capture

    @property
    def destinations(self) -> set[Location]:
        return {self._location + delta for delta in self._movement.get_deltas()}

    def can_move(self, dest: Location) -> bool:
        return dest - self._location in self._movement.get_deltas()

    def move(self, dest: Location) -> None:
        if not self.can_move(dest):
            raise ValueError(f'Error: ProtectedPiece cannot move to square {dest}')

        self._location = copy.copy(dest)


# --- MARK: PieceFactory


class PieceFactory(Protocol):
    @classmethod
    def make(
        cls, player: Player, piece_kind: PieceKind, location: Location
    ) -> Piece: ...


class BoardGamePieceFactory:
    @classmethod
    def make(cls, player: Player, piece_kind: PieceKind, location: Location) -> Piece:
        match piece_kind:
            case PieceKind.PAWN:
                return RegularPiece(player, piece_kind, location, PawnMovement(player))
            case PieceKind.GRAIL:
                return RegularPiece(player, piece_kind, location, GrailMovement())
            case PieceKind.LANCE:
                return RegularPiece(player, piece_kind, location, LanceMovement())
            case PieceKind.FLAG_LEFT:
                return ProtectedPiece(
                    player, piece_kind, location, FlagLeftMovement(player)
                )
            case PieceKind.FLAG_RIGHT:
                return ProtectedPiece(
                    player, piece_kind, location, FlagRightMovement(player)
                )
            case PieceKind.SWORD:
                return RegularPiece(player, piece_kind, location, SwordMovement(player))
            case PieceKind.BOW:
                return RegularPiece(player, piece_kind, location, BowMovement(player))
            case PieceKind.DAGGER:
                return RegularPiece(
                    player, piece_kind, location, DaggerMovement(player)
                )


# --- MARK: PiecePositions


class PiecePositions(Protocol):
    def get_positions(self) -> dict[Location, tuple[Player, PieceKind]]: ...


class BoardGamePiecePositions:
    def get_positions(self) -> dict[Location, tuple[Player, PieceKind]]:
        return {
            # Player 1
            ### Row 1
            Location(1, 1): (Player.PLAYER_1, PieceKind.FLAG_RIGHT),
            Location(1, 3): (Player.PLAYER_1, PieceKind.GRAIL),
            Location(1, 4): (Player.PLAYER_1, PieceKind.BOW),
            Location(1, 7): (Player.PLAYER_1, PieceKind.BOW),
            Location(1, 8): (Player.PLAYER_1, PieceKind.GRAIL),
            Location(1, 10): (Player.PLAYER_1, PieceKind.FLAG_LEFT),
            ### Row 2
            Location(2, 3): (Player.PLAYER_1, PieceKind.SWORD),
            Location(2, 4): (Player.PLAYER_1, PieceKind.DAGGER),
            Location(2, 5): (Player.PLAYER_1, PieceKind.LANCE),
            Location(2, 6): (Player.PLAYER_1, PieceKind.LANCE),
            Location(2, 7): (Player.PLAYER_1, PieceKind.DAGGER),
            Location(2, 8): (Player.PLAYER_1, PieceKind.SWORD),
            ### Row 3
            Location(3, 1): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 2): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 3): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 4): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 5): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 6): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 7): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 8): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 9): (Player.PLAYER_1, PieceKind.PAWN),
            Location(3, 10): (Player.PLAYER_1, PieceKind.PAWN),
            # Player 2
            ### Row 8
            Location(8, 1): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 2): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 3): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 4): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 5): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 6): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 7): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 8): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 9): (Player.PLAYER_2, PieceKind.PAWN),
            Location(8, 10): (Player.PLAYER_2, PieceKind.PAWN),
            ### Row 9
            Location(9, 3): (Player.PLAYER_2, PieceKind.SWORD),
            Location(9, 4): (Player.PLAYER_2, PieceKind.DAGGER),
            Location(9, 5): (Player.PLAYER_2, PieceKind.LANCE),
            Location(9, 6): (Player.PLAYER_2, PieceKind.LANCE),
            Location(9, 7): (Player.PLAYER_2, PieceKind.DAGGER),
            Location(9, 8): (Player.PLAYER_2, PieceKind.SWORD),
            ### Row 10
            Location(10, 1): (Player.PLAYER_2, PieceKind.FLAG_RIGHT),
            Location(10, 3): (Player.PLAYER_2, PieceKind.GRAIL),
            Location(10, 4): (Player.PLAYER_2, PieceKind.BOW),
            Location(10, 7): (Player.PLAYER_2, PieceKind.BOW),
            Location(10, 8): (Player.PLAYER_2, PieceKind.GRAIL),
            Location(10, 10): (Player.PLAYER_2, PieceKind.FLAG_LEFT),
        }


# --- MARK: Board


@dataclass
class Board:
    _rows: int
    _columns: int
    _pieces: dict[Location, Piece] = field(init=False, default_factory=dict)

    @property
    def rows(self) -> int:
        return self._rows

    @property
    def columns(self) -> int:
        return self._columns

    @property
    def pieces(self) -> dict[Location, Piece]:
        return self._pieces

    def add_piece(self, piece: Piece) -> None:
        if not self.is_square_within_bounds(piece.location):
            raise KeyError(
                'Error: Attempted to add a piece to an out of bounds location on the board.'
            )

        if not self.is_square_empty(piece.location):
            raise KeyError(
                'Error: Attempted to add a piece to a non-empty board square.'
            )

        self._pieces[piece.location] = piece

    def get_piece(self, location: Location) -> Piece | None:
        if not self.is_square_within_bounds(location):
            raise KeyError(
                'Error: Attempted to get a piece from an out of bounds location on the board.'
            )

        return self._pieces.get(location, None)

    def remove_piece(self, location: Location) -> None:
        if not self.is_square_within_bounds(location):
            raise KeyError(
                'Error: Attempted to remove a piece from an out of bounds location on the board.'
            )

        if self.is_square_empty(location):
            raise KeyError(
                'Error: Attempted to remove a piece from an empty board square.'
            )

        self._pieces.pop(location)

    def is_square_empty(self, location: Location) -> bool:
        return location not in self._pieces

    def is_square_within_bounds(self, location: Location) -> bool:
        return 1 <= location.row <= self._rows and 1 <= location.column <= self._columns


# --- MARK: BoardSetter


class BoardSetter:
    _piece_positions: PiecePositions
    _piece_factory: PieceFactory

    def __init__(
        self, piece_positions: PiecePositions, piece_factory: PieceFactory
    ) -> None:
        self._piece_positions = piece_positions
        self._piece_factory = piece_factory

    def setup_board(self, board: Board):
        for (
            location,
            player_piece_kind,
        ) in self._piece_positions.get_positions().items():
            player: Player = player_piece_kind[0]
            piece_kind: PieceKind = player_piece_kind[1]
            piece: Piece = self._piece_factory.make(player, piece_kind, location)
            board.add_piece(piece)


# --- MARK: BoardGameFrozenGameState


@dataclass(frozen=True)
class BoardGameFrozenGameState:
    _max_moves: int
    _player_to_move: Player
    _turn: int
    _move: int

    @property
    def max_moves(self) -> int:
        return self._max_moves

    @property
    def player_to_move(self) -> Player:
        return self._player_to_move

    @property
    def turn(self) -> int:
        return self._turn

    @property
    def move(self) -> int:
        return self._move


# --- MARK: Board Game Model


class BoardGameModel:
    # non-state "constants"
    _player: Player
    # state
    _board: Board
    _captured_pieces: dict[Player, dict[PieceKind, int]]
    _state: BoardGameFrozenGameState
    # non-state extras
    _board_setter: BoardSetter

    @classmethod
    def setup_game(cls, player_id: PlayerId) -> BoardGameModel:
        MAX_MOVES: int = 3
        board: Board = Board(10, 10)
        captured_pieces: dict[Player, dict[PieceKind, int]] = {
            Player.PLAYER_1: {},
            Player.PLAYER_2: {},
        }
        state: BoardGameFrozenGameState = BoardGameFrozenGameState(
            _max_moves=MAX_MOVES, _player_to_move=Player.PLAYER_1, _turn=1, _move=1
        )

        return cls(
            player_id,
            board,
            captured_pieces,
            state,
            BoardGamePiecePositions(),
            BoardGamePieceFactory(),
        )

    def __init__(
        self,
        player_id: PlayerId,
        board: Board,
        captured_pieces: dict[Player, dict[PieceKind, int]],
        state: BoardGameFrozenGameState,
        piece_positions: PiecePositions,
        piece_factory: PieceFactory,
    ) -> None:
        match player_id:
            case 1:
                self._player = Player.PLAYER_1
            case 2:
                self._player = Player.PLAYER_2
            case _:
                raise ValueError('Error: BoardGameModel received invalid player ID.')
        self._board = board
        self._captured_pieces = captured_pieces
        self._state = state
        self._board_setter = BoardSetter(piece_positions, piece_factory)

        self._board_setter.setup_board(self._board)

    @property
    def player(self) -> Player:
        return self._player

    @property
    def max_moves(self) -> int:
        return self._state.max_moves

    @property
    def board(self) -> Board:
        return self._board

    @property
    def captured_pieces(self) -> dict[Player, dict[PieceKind, int]]:
        return self._captured_pieces

    @property
    def player_to_move(self) -> Player:
        return self._state.player_to_move

    @property
    def turn(self) -> int:
        return self._state.turn

    @property
    def move(self) -> int:
        return self._state.move

    @property
    def game_status(self) -> GameStatus:
        match (
            self._can_any_protected_piece_move(Player.PLAYER_1),
            self._can_any_protected_piece_move(Player.PLAYER_2),
        ):
            case False, False:
                return GameStatus.DRAW
            case False, True:
                return GameStatus.PLAYER_2_WIN
            case True, False:
                return GameStatus.PLAYER_1_WIN
            case True, True:
                return GameStatus.ONGOING

    @property
    def protected_pieces(self) -> dict[Player, list[Piece]]:
        ret: dict[Player, list[Piece]] = {}

        for piece in self._board.pieces.values():
            if piece.player not in ret:
                ret[piece.player] = []

            if piece.is_protected:
                ret[piece.player].append(piece)

        return ret

    def _can_any_protected_piece_move(self, player: Player) -> bool:
        if player not in self.protected_pieces:
            return False

        for piece in self.protected_pieces[player]:
            for dest in piece.destinations:
                if self.is_move_valid(piece.location, dest, None):
                    return True

        return False

    def new_game(self) -> None:
        self._board_setter.setup_board(self._board)
        self._captured_pieces = {
            Player.PLAYER_1: {},
            Player.PLAYER_2: {},
        }

        self._state = replace(
            self._state, _player_to_move=Player.PLAYER_1, _turn=1, _move=1
        )

    def next_move(self) -> None:
        # update turn/move number
        if self.move < self.max_moves:
            self._state = replace(
                self._state,
                _move=self._state.move + 1,
            )
        else:
            self._state = replace(
                self._state,
                _turn=self._state.turn + 1,
                _move=1,
            )

            # change player to move
            match self.player_to_move:
                case Player.PLAYER_1:
                    self._state = replace(self._state, _player_to_move=Player.PLAYER_2)
                case Player.PLAYER_2:
                    self._state = replace(self._state, _player_to_move=Player.PLAYER_1)

    def get_move_feedback_info(
        self, src: Location, dest: Location, player: Player | None
    ) -> MoveFeedbackInfo:
        # not currently player's turn to move
        # skip this condition if player is not specified (to check "hypothetical" moves)
        if player is not None and self._state.player_to_move != player:
            return MoveFeedbackInfo.NOT_CURRENT_PLAYER

        # src Location does not exist in board
        if not self._board.is_square_within_bounds(src):
            return MoveFeedbackInfo.SQUARE_OUT_OF_BOUNDS

        # dest Location does not exist in board
        if not self._board.is_square_within_bounds(dest):
            return MoveFeedbackInfo.SQUARE_OUT_OF_BOUNDS

        # ---

        src_piece: Piece | None = self._board.get_piece(src)

        # no piece exists in src Location
        if src_piece is None:
            return MoveFeedbackInfo.NO_PIECE_MOVED

        # piece in src Location does not belong to the player
        # skip this condition if player is not specified (to check "hypothetical" moves)
        if player is not None and src_piece.player != player:
            return MoveFeedbackInfo.PIECE_DOES_NOT_BELONG_TO_PLAYER

        # piece in src Location cannot reach dest Location
        if not src_piece.can_move(dest):
            return MoveFeedbackInfo.PIECE_CANNOT_REACH_SQUARE

        # ---

        dest_piece: Piece | None = self._board.get_piece(dest)

        # a piece can always move to an empty dest Location
        if dest_piece is None:
            return MoveFeedbackInfo.VALID

        # a player cannot capture their own piece
        if src_piece.player == dest_piece.player:
            return MoveFeedbackInfo.CAPTURES_OWN_PIECE
        # a player can capture an opponent piece if it is not a protected piece
        else:
            if dest_piece.is_protected:
                return MoveFeedbackInfo.CAPTURES_PROTECTED_PIECE
            elif not src_piece.can_capture:
                return MoveFeedbackInfo.PIECE_CANNOT_CAPTURE
            else:
                return MoveFeedbackInfo.VALID

    def is_move_valid(
        self, src: Location, dest: Location, player: Player | None
    ) -> bool:
        return self.get_move_feedback_info(src, dest, player) == MoveFeedbackInfo.VALID

    def _make_valid_move_feedback(
        self, src: Location, dest: Location, player: Player
    ) -> MoveFeedback:
        return MoveFeedback(
            move_src=src,
            move_dest=dest,
            info=self.get_move_feedback_info(src, dest, player),
        )

    def _make_invalid_move_feedback(
        self, src: Location, dest: Location, player: Player
    ) -> MoveFeedback:
        return MoveFeedback(
            move_src=src,
            move_dest=None,
            info=self.get_move_feedback_info(src, dest, player),
        )

    def move_piece(self, src: Location, dest: Location, player: Player) -> MoveFeedback:
        if not self.is_move_valid(src, dest, player):
            return self._make_invalid_move_feedback(src, dest, player)
        else:
            ret: MoveFeedback = self._make_valid_move_feedback(src, dest, player)

            # ---

            src_piece: Piece | None = self._board.get_piece(src)
            if src_piece is None:
                raise Exception(f'Error: Move was called from empty square {src}.')

            # ---

            dest_piece: Piece | None = self._board.get_piece(dest)
            if dest_piece is not None:
                match dest_piece.player:
                    case Player.PLAYER_1:
                        receiving_player: Player = Player.PLAYER_2
                    case Player.PLAYER_2:
                        receiving_player: Player = Player.PLAYER_1

                # add piece to captured pieces
                self._captured_pieces[receiving_player][dest_piece.piece_kind] = (
                    self._captured_pieces[receiving_player].setdefault(
                        dest_piece.piece_kind, 0
                    )
                    + 1
                )

                # remove captured piece from board
                self._board.remove_piece(dest)

            # ---

            # update piece with new position
            src_piece.move(dest)

            # add piece to new position
            self._board.add_piece(src_piece)

            # clear old position from board
            self._board.remove_piece(src)

            # ---

            self.next_move()

            return ret

    def get_place_feedback_info(
        self, piece_kind: PieceKind, dest: Location, player: Player | None
    ) -> PlaceFeedbackInfo:
        # not currently player's turn to place
        # skip this condition if player is not specified (to check "hypothetical" places)
        if player is not None and self._state.player_to_move != player:
            return PlaceFeedbackInfo.NOT_CURRENT_PLAYER

        # dest Location does not exist in board
        if not self._board.is_square_within_bounds(dest):
            return PlaceFeedbackInfo.SQUARE_OUT_OF_BOUNDS

        # ---

        # player not in captured pieces dict (this should never happen)
        if player not in self._captured_pieces:
            return PlaceFeedbackInfo.NO_PLAYER_PLAYED

        # player has 0 of a selected piece to place
        if self._captured_pieces[player].get(piece_kind, 0) == 0:
            return PlaceFeedbackInfo.NO_PIECE_PLACED

        # ---

        dest_piece: Piece | None = self._board.get_piece(dest)
        # a piece can be placed on an empty dest location if no enemy protected piece can go to it
        if dest_piece is None:
            for protected_pieces in self.protected_pieces.values():
                for protected_piece in protected_pieces:
                    if self.is_move_valid(protected_piece.location, dest, None):
                        return PlaceFeedbackInfo.BLOCKS_PROTECTED_PIECE_MOVEMENT

            return PlaceFeedbackInfo.VALID
        # a piece cannot be placed on an occupied dest Location
        else:
            return PlaceFeedbackInfo.CAPTURES_PIECE

    def is_place_valid(
        self,
        piece_kind: PieceKind,
        dest: Location,
        player: Player | None,
    ) -> bool:
        return (
            self.get_place_feedback_info(piece_kind, dest, player)
            == PlaceFeedbackInfo.VALID
        )

    def _make_valid_place_feedback(
        self, piece_kind: PieceKind, dest: Location, player: Player
    ) -> PlaceFeedback:
        return PlaceFeedback(
            place_piece_kind=piece_kind,
            place_dest=dest,
            info=self.get_place_feedback_info(piece_kind, dest, player),
        )

    def _make_invalid_place_feedback(
        self, piece_kind: PieceKind, dest: Location, player: Player
    ) -> PlaceFeedback:
        return PlaceFeedback(
            place_piece_kind=piece_kind,
            place_dest=dest,
            info=self.get_place_feedback_info(piece_kind, dest, player),
        )

    def place_piece(
        self, piece_kind: PieceKind, dest: Location, player: Player
    ) -> PlaceFeedback:
        if not self.is_place_valid(piece_kind, dest, player):
            return self._make_invalid_place_feedback(piece_kind, dest, player)
        else:
            ret: PlaceFeedback = self._make_valid_place_feedback(
                piece_kind, dest, player
            )

            # make new piece
            new_piece: Piece = BoardGamePieceFactory.make(player, piece_kind, dest)

            # remove piece from captured pieces
            self._captured_pieces[player][piece_kind] -= 1

            # add piece to board
            self._board.add_piece(new_piece)

            self.next_move()

            return ret
