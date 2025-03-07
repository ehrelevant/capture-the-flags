from __future__ import annotations
from math import ceil
import os
import pygame
from pygame import Rect, Surface, Clock, Font
from pygame.sprite import Group, Sprite
import sys
from typing import Generator, NoReturn, Protocol, Mapping
from cs150241project_networking import CS150241ProjectNetworking, Message
from project_types import (
    Player,
    PieceKind,
    Location,
    PieceData,
    GameState,
    MoveFeedback,
    MoveFeedbackInfo,
    PlaceFeedback,
    PlaceFeedbackInfo,
    GameStatus,
)
# import random

# --- MARK: Constants
REL_CAPTURED_BOX_WIDTH = 0.15
REL_TEXT_MARGIN = 20
SCREEN_WIDTH = 900
SCREEN_HEIGHT = 700
FPS = 60


# --- MARK: Position
# Position class is determining where an object is in the screen
class Position:
    _x: int
    _y: int

    @classmethod
    def from_tuple(cls, tup: tuple[int, int]) -> Position:
        return cls(tup[0], tup[1])

    def __init__(self, x: int, y: int) -> None:
        self._x = x
        self._y = y

    def __add__(self, other: Position | tuple[int, int]):
        if isinstance(other, Position):
            return Position(self.x + other.x, self.y + other.y)
        elif type(other) is tuple and list(map(type, other)) == [int, int]:
            return Position(self.x + other[0], self.y + other[1])
        else:
            raise TypeError('Error: Invalid type for __add__ with Position()')

    def __iter__(self) -> Generator[int]:
        yield self._x
        yield self._y

    def __repr__(self) -> str:
        return f'{self._x, self._y}'

    def __rsub__(self, other: Position | tuple[int, int]):
        if isinstance(other, Position):
            return Position(other.x - self.x, other.y - self.y)
        elif type(other) is tuple and list(map(type, other)) == [int, int]:
            return Position(other[0] - self.x, other[1] - self.y)
        else:
            raise TypeError('Error: Invalid type for __rsub__ with Position()')

    def __sub__(self, other: Position | tuple[int, int]):
        if isinstance(other, Position):
            return Position(self.x - other.x, self.y - other.y)
        elif type(other) is tuple and list(map(type, other)) == [int, int]:
            return Position(self.x - other[0], self.y - other[1])
        else:
            raise TypeError('Error: Invalid type for __sub__ with Position()')

    @property
    def x(self) -> int:
        return self._x

    @property
    def y(self) -> int:
        return self._y


# --- MARK: Piece
class Piece(Sprite):
    _piece_kind: PieceKind
    _location: Location
    _position: Position
    _last_stable_position: Position
    _size: int
    _owned_by: Player

    def __init__(
        self,
        piece_kind: PieceKind,
        location: Location,
        image: SpriteImage,
        position: Position,
        size: int,
        owned_by: Player,
        client_by: Player,
    ):
        super().__init__()
        self._piece_kind = piece_kind
        self._location = location
        self._owned_by = owned_by

        # flip image for Player 2
        self.image = (
            pygame.transform.scale(image.get_sprite(self._owned_by), (size, size))
            if client_by == Player.PLAYER_1
            else pygame.transform.flip(
                pygame.transform.scale(image.get_sprite(self._owned_by), (size, size)),
                True,
                False,
            )
        )

        self._position = position
        self._last_stable_position = self._position
        self._size = size
        self.rect: Rect = pygame.Rect(
            position.x,
            position.y,
            self._size,
            self._size,
        )

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def location(self) -> Location:
        return self._location

    @property
    def position(self) -> Position:
        return self._position

    @property
    def last_stable_position(self) -> Position:
        return self._last_stable_position

    @property
    def owned_by(self) -> Player:
        return self._owned_by

    def update_position(self) -> None:
        self.rect.x = self._position.x
        self.rect.y = self._position.y

    # move both image and collision box using the relative position argument
    def move_rel(self, rel_position: tuple[int, int]) -> None:
        # update self._position
        self._position += rel_position

    def move_abs(self, abs_position: tuple[int, int]) -> None:
        # overwrite self._position
        self._position = Position.from_tuple(abs_position)

    def snap(self, cell_to_snap: Rect) -> None:
        self.rect.clamp_ip(cell_to_snap)
        self._position = Position(cell_to_snap.x, cell_to_snap.y)
        # update new stable position
        self._last_stable_position = self._position

    # return piece to previous spot
    def reset_to_spot(self) -> None:
        self.rect.update(
            (self._last_stable_position.x, self._last_stable_position.y),
            (self._size, self._size),
        )
        self._position = self._last_stable_position


# --- MARK: Grid
class Grid:
    _relative_width: int
    _relative_height: int
    _dim_x: int
    _dim_y: int
    _cell_length: int
    _grid: list[list[Rect]]
    _player: Player
    _margin: int

    def __init__(
        self, s_w: int, s_h: int, dim_x: int, dim_y: int, player: Player
    ) -> None:
        self._relative_width = s_w - int(REL_CAPTURED_BOX_WIDTH * s_w)
        self._relative_height = s_h
        self._dim_x = dim_x
        self._dim_y = dim_y
        self._margin = s_h // REL_TEXT_MARGIN
        self._cell_length = min(
            (self._relative_height - (2 * self._margin)) // dim_x,
            (self._relative_width - (2 * self._margin)) // dim_y,
        )
        self._grid = []
        self._player = player

        self._create_grid(dim_x, dim_y)

    @property
    def cell_length(self) -> int:
        return self._cell_length

    def _create_grid(self, row: int, col: int) -> None:
        for i in range(row):
            temp_row: list[Rect] = []
            for j in range(col):
                # append cell to row
                temp_row.append(
                    pygame.Rect(
                        self.center_align(j, col, self._cell_length, True),
                        self.center_align(i, row, self._cell_length, False),
                        self._cell_length,
                        self._cell_length,
                    )
                )

            # append row to grid (dependent on Player ID)
            match self._player:
                case Player.PLAYER_1:
                    self._grid.insert(0, temp_row)
                case Player.PLAYER_2:
                    temp_row.reverse()
                    self._grid.append(temp_row)

    def center_align(self, num: int, dimension: int, length: int, is_x: bool) -> int:
        center_screen: int = (
            self._relative_width // 2 if is_x else self._relative_height // 2
        )
        center_index: int

        # odd center
        if dimension % 2 != 0:
            center_index = dimension // 2 + 1
            return (center_screen + (length // 2)) + (length * (num - center_index))
        # even center
        else:
            center_index = dimension // 2
            return (center_screen) + (length * (num - center_index))

    def render(self, screen: Surface) -> None:
        for ith, row in enumerate(self._grid):
            for jth, col in enumerate(row):
                # following chess.com format (LOL)
                if (ith % 2 == 0 and jth % 2 == 0) or (ith % 2 != 0 and jth % 2 != 0):
                    pygame.draw.rect(screen, 'lightyellow', col)
                else:
                    pygame.draw.rect(screen, 'lightgreen', col)

    # event.pos is considered as Any type
    def snap_position(self, cursor_position: tuple[int, int]) -> Rect | None:
        for row in self._grid:
            for cell in row:
                if cell.collidepoint(cursor_position):
                    return cell

    # --- Grid conversions

    # note: location is 1-indexed
    def get_cell_from_location(self, loc: Location) -> Rect:
        return self._grid[loc.row - 1][loc.column - 1]

    def get_location_from_position(self, pos: Position) -> Location:
        zero_zero_location: Rect = self._grid[0][0]
        match self._player:
            case Player.PLAYER_1:
                return Location(
                    ((zero_zero_location.y - pos.y) // self._cell_length) + 1,
                    ((pos.x - zero_zero_location.x) // self._cell_length) + 1,
                )
            case Player.PLAYER_2:
                # return location based on player_id (due to a board rotated 180 degrees)
                return Location(
                    ((pos.y - zero_zero_location.y) // self._cell_length) + 1,
                    ((zero_zero_location.x - pos.x) // self._cell_length) + 1,
                )

    def get_position_from_cell(self, cell: Rect) -> Position:
        return Position(cell.x, cell.y)

    # function composition
    def get_location_from_cell(self, cell: Rect) -> Location:
        return self.get_location_from_position(self.get_position_from_cell(cell))

    def get_position_from_location(self, loc: Location) -> Position:
        return self.get_position_from_cell(self.get_cell_from_location(loc))

    def get_cell_from_position(self, pos: Position) -> Rect:
        return self.get_cell_from_location(self.get_location_from_position(pos))


# --- MARK: Button
class Button:
    _s: str
    _position: Position
    _bg_c: str
    _txt_c: str
    _render: Surface
    _rect: Rect

    def __init__(
        self, s: str, position: Position, bg_color: str, txt_color: str, font: Font
    ) -> None:
        self._s = s
        self._position = position
        self._bg_c = bg_color
        self._txt_c = txt_color

        self._render = font.render(self._s, True, self._txt_c, self._bg_c)
        self._rect = self._render.get_rect()
        self._rect.center = tuple(self._position)

    @property
    def content(self) -> str:
        return self._s

    @property
    def collision_box(self) -> Rect:
        return self._rect

    def update_text(self, s: str) -> None:
        self._s = s

    def update_text_color(self, color: str) -> None:
        self._txt_c = color

    def update_bg(self, color: str) -> None:
        self._bg_c = color

    def render(self, screen: Surface, font: Font) -> None:
        self._render: Surface = font.render(self._s, True, self._txt_c, self._bg_c)
        screen.blit(self._render, self._rect)


# --- MARK: Captured Pieces
class CapturedPiece(Sprite):
    _piece_kind: PieceKind
    _size: int
    _position: Position
    _last_stable_position: Position
    _image: Surface

    def __init__(
        self,
        piece: PieceKind,
        image: SpriteImage,
        position: Position,
        size: int,
        owned_by: Player,
    ):
        super().__init__()
        self._piece_kind = piece
        self._size = size
        self.rect: Rect = pygame.Rect(
            position.x,
            position.y,
            self._size,
            self._size,
        )

        self._position = position
        self._last_stable_position = position
        # flip image for Player 2
        self.image = (
            pygame.transform.scale(image.get_sprite(owned_by), (size, size))
            if owned_by == Player.PLAYER_1
            else pygame.transform.flip(
                pygame.transform.scale(image.get_sprite(owned_by), (size, size)),
                True,
                False,
            )
        )

    @property
    def piece_kind(self) -> PieceKind:
        return self._piece_kind

    @property
    def size(self) -> int:
        return self._size

    def update_position(self) -> None:
        self.rect.x = self._position.x
        self.rect.y = self._position.y

    # move both image and collision box using the relative position argument
    def move_rel(self, rel_position: tuple[int, int]) -> None:
        self.rect.move_ip(rel_position)
        self._position += rel_position

    def reset_to_spot(self) -> None:
        self.rect.update(
            tuple(self._last_stable_position),
            (self._size, self._size),
        )
        self._position = self._last_stable_position


# --- MARK: Capture Box
class CaptureBox:
    MAX_PIECES: int = 4
    _width: int
    _height: int
    _position: Position
    _container: Rect
    _capture_list_factory: CapturedPieceFactory
    _capture_list: list[CapturedPiece]
    _buttons: list[Button]
    _slice: tuple[int, int]
    _page: int
    _player: Player

    def __init__(self, font: Font, player: Player) -> None:
        self._width = int(SCREEN_WIDTH * REL_CAPTURED_BOX_WIDTH)
        self._height = SCREEN_HEIGHT

        self._position = Position(SCREEN_WIDTH - self._width, 0)

        self._container = pygame.Rect(
            SCREEN_WIDTH - self._width,
            0,
            self._width,
            self._height,
        )

        self._capture_list = []
        self._capture_list_factory = CapturedPieceDefaultFactory()
        self._slice = (0, CaptureBox.MAX_PIECES)  # the list shows at most 4 pieces
        self._page = 1
        self._player = player

        self._buttons = [
            Button(
                '<< Prev',
                Position(
                    self._position.x + (self._width // 2),
                    self._height - int(self._height * 0.15),
                ),
                'sandybrown' if player == Player.PLAYER_1 else 'deepskyblue1',
                'black',
                font,
            ),
            Button(
                'Next >>',
                Position(
                    self._position.x + (self._width // 2),
                    self._height - int(self._height * 0.1),
                ),
                'sandybrown' if player == Player.PLAYER_1 else 'deepskyblue1',
                'black',
                font,
            ),
        ]

    @property
    def buttons(self) -> list[Button]:
        return self._buttons

    @property
    def capture_list(self) -> list[CapturedPiece]:
        return self._capture_list

    @property
    def shown_capture_list(self) -> list[CapturedPiece]:
        return self._capture_list[self._slice[0] : self._slice[1]]

    @property
    def start_of_slice(self) -> int:
        return self._slice[0]

    def render(self, screen: Surface, font: Font) -> None:
        pygame.draw.rect(
            screen,
            'sandybrown' if self._player == Player.PLAYER_1 else 'deepskyblue1',
            self._container,
        )

        # render header
        header_render: Surface = font.render(
            'Captures',
            True,
            'black',
            'sandybrown' if self._player == Player.PLAYER_1 else 'deepskyblue1',
        )
        header_rect: Rect = header_render.get_rect()
        header_rect.center = (
            self._position.x + (self._width // 2),
            self._position.y + 20,
        )
        screen.blit(header_render, header_rect)

        group_capture_sprites: Group[Sprite] = Group()

        for piece in self._capture_list[self._slice[0] : self._slice[1]]:
            piece.update_position()
            group_capture_sprites.add(piece)

        group_capture_sprites.draw(screen)

        for button in self._buttons:
            button.render(screen, font)

        # render footer
        footer_render: Surface = font.render(
            f'Page {self._page} of {1 if len(self._capture_list) == 0 else ceil(len(self._capture_list) / 4)}',
            True,
            'black',
            'sandybrown' if self._player == Player.PLAYER_1 else 'deepskyblue1',
        )
        footer_rect: Rect = footer_render.get_rect()
        footer_rect.center = (self._position.x + (self._width // 2), self._height - 20)
        screen.blit(footer_render, footer_rect)

    def add_captured_piece(self, pk: PieceKind) -> None:
        # create captured piece using piecefactory (OCP compliance)
        self._capture_list.append(
            self._capture_list_factory.make(
                pk,
                Position(
                    self._position.x
                    + (self._width // 2)
                    - (int(self._width * 0.5) // 2),
                    int(
                        (self._height * 0.1)
                        + (len(self._capture_list) % 4) * self._height * 0.175
                    ),
                ),
                int(self._width * 0.5),
                self._player,
            )
        )

    def move_piece(self, rel: tuple[int, int], index: int) -> None:
        self._capture_list[index].move_rel(rel)

    def go_to_page(self, button_index: int) -> None:
        # previous page
        if button_index == 0:
            if self._slice[0] == 0:
                # do nothing
                return
            else:
                # update slice
                self._slice = (self._slice[0] - 4, self._slice[1] - 4)
                self._page -= 1
        # next page
        else:
            if self._slice[1] >= len(self._capture_list):
                # do nothing
                return
            else:
                # update slice
                self._slice = (self._slice[0] + 4, self._slice[1] + 4)
                self._page += 1

        return

    def update_captured_list(
        self, captured_dict: Mapping[Player, Mapping[PieceKind, int]], player: Player
    ) -> None:
        # clear the current captured list
        self.capture_list.clear()

        # iterate through game state `captured_pieces` for the client (only show one player's captured pieces)
        for pks in captured_dict[player].keys():
            for _ in range(captured_dict[player][pks]):
                self.add_captured_piece(pks)

        return

    def reset_captured_pieces(self) -> None:
        for piece in self._capture_list:
            piece.reset_to_spot()


# --- MARK: Observers
class MovePieceObserver(Protocol):
    def on_move_piece(self, old: Location, new: Location, player: Player): ...


class PlacePieceObserver(Protocol):
    def on_place_piece(self, piece_kind: PieceKind, dest: Location, player: Player): ...


class ReceiveMessageObserver(Protocol):
    def on_receive_message(self, message: Message): ...


class GameStateObserver(Protocol):
    def on_state_change(self, state: GameState): ...


# --- MARK: Sprite
class SpriteImage(Protocol):
    def get_sprite(self, player: Player) -> Surface: ...


class PawnSprite:
    def get_sprite(self, player: Player) -> Surface:
        match player:
            case Player.PLAYER_1:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'white', 'pawn.png')
                )
            case Player.PLAYER_2:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'black', 'pawn.png')
                )


class LanceSprite:
    def get_sprite(self, player: Player) -> Surface:
        match player:
            case Player.PLAYER_1:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'white', 'lance.png')
                )
            case Player.PLAYER_2:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'black', 'lance.png')
                )


class GrailSprite:
    def get_sprite(self, player: Player) -> Surface:
        match player:
            case Player.PLAYER_1:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'white', 'grail.png')
                )
            case Player.PLAYER_2:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'black', 'grail.png')
                )


class FlagLeftSprite:
    def get_sprite(self, player: Player) -> Surface:
        match player:
            case Player.PLAYER_1:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'white', 'flag-left.png')
                )
            case Player.PLAYER_2:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'black', 'flag-left.png')
                )


class FlagRightSprite:
    def get_sprite(self, player: Player) -> Surface:
        match player:
            case Player.PLAYER_1:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'white', 'flag-right.png')
                )
            case Player.PLAYER_2:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'black', 'flag-right.png')
                )


class SwordSprite:
    def get_sprite(self, player: Player) -> Surface:
        match player:
            case Player.PLAYER_1:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'white', 'sword.png')
                )
            case Player.PLAYER_2:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'black', 'sword.png')
                )


class BowSprite:
    def get_sprite(self, player: Player) -> Surface:
        match player:
            case Player.PLAYER_1:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'white', 'bow.png')
                )
            case Player.PLAYER_2:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'black', 'bow.png')
                )


class DaggerSprite:
    def get_sprite(self, player: Player) -> Surface:
        match player:
            case Player.PLAYER_1:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'white', 'dagger.png')
                )
            case Player.PLAYER_2:
                return pygame.image.load(
                    os.path.join('src', 'assets', 'black', 'dagger.png')
                )


# --- MARK: PieceFactories
class BoardPieceFactory(Protocol):
    @classmethod
    def make(
        cls,
        pk: PieceKind,
        position: Position,
        size: int,
        player: Player,
        location: Location,
        client_player: Player,
    ) -> Piece: ...


class BoardPieceDefaultFactory:
    @classmethod
    def make(
        cls,
        pk: PieceKind,
        position: Position,
        size: int,
        player: Player,
        location: Location,
        client_player: Player,
    ) -> Piece:
        match pk:
            case PieceKind.PAWN:
                return Piece(
                    pk, location, PawnSprite(), position, size, player, client_player
                )
            case PieceKind.GRAIL:
                return Piece(
                    pk, location, GrailSprite(), position, size, player, client_player
                )
            case PieceKind.LANCE:
                return Piece(
                    pk, location, LanceSprite(), position, size, player, client_player
                )
            case PieceKind.FLAG_LEFT:
                return Piece(
                    pk,
                    location,
                    FlagLeftSprite(),
                    position,
                    size,
                    player,
                    client_player,
                )
            case PieceKind.FLAG_RIGHT:
                return Piece(
                    pk,
                    location,
                    FlagRightSprite(),
                    position,
                    size,
                    player,
                    client_player,
                )
            case PieceKind.SWORD:
                return Piece(
                    pk, location, SwordSprite(), position, size, player, client_player
                )
            case PieceKind.BOW:
                return Piece(
                    pk, location, BowSprite(), position, size, player, client_player
                )
            case PieceKind.DAGGER:
                return Piece(
                    pk, location, DaggerSprite(), position, size, player, client_player
                )


class CapturedPieceFactory(Protocol):
    @classmethod
    def make(
        cls, pk: PieceKind, position: Position, size: int, player: Player
    ) -> CapturedPiece: ...


class CapturedPieceDefaultFactory:
    @classmethod
    def make(
        cls, pk: PieceKind, position: Position, size: int, player: Player
    ) -> CapturedPiece:
        match pk:
            case PieceKind.PAWN:
                return CapturedPiece(pk, PawnSprite(), position, size, player)
            case PieceKind.GRAIL:
                return CapturedPiece(pk, GrailSprite(), position, size, player)
            case PieceKind.LANCE:
                return CapturedPiece(pk, LanceSprite(), position, size, player)
            case PieceKind.FLAG_LEFT:
                return CapturedPiece(pk, FlagLeftSprite(), position, size, player)
            case PieceKind.FLAG_RIGHT:
                return CapturedPiece(pk, FlagRightSprite(), position, size, player)
            case PieceKind.SWORD:
                return CapturedPiece(pk, SwordSprite(), position, size, player)
            case PieceKind.BOW:
                return CapturedPiece(pk, BowSprite(), position, size, player)
            case PieceKind.DAGGER:
                return CapturedPiece(pk, DaggerSprite(), position, size, player)


# --- MARK: BoardGameView
class BoardGameView:
    _width: int
    _height: int
    _fps: int
    _screen: Surface
    _font: Font
    _clock: Clock
    _frame_count: int
    _pieces: dict[Location, Piece]
    _piece_factory: BoardPieceFactory
    _grid: Grid
    _capture_box: CaptureBox
    _current_player: Player
    _player: Player

    _active_cell_to_snap: Rect | None
    _game_status: GameStatus

    # observers
    _move_piece_observers: list[MovePieceObserver]
    _place_piece_observers: list[PlacePieceObserver]
    _receive_message_observers: list[ReceiveMessageObserver]

    def __init__(self, state: GameState) -> None:
        pygame.init()

        self._width = SCREEN_WIDTH
        self._height = SCREEN_HEIGHT
        self._fps = FPS
        self._screen = pygame.display.set_mode((self._width, self._height))
        self._font = pygame.font.SysFont('', 30)
        self._clock = pygame.time.Clock()
        self._frame_count = 0
        self._pieces = {}
        self._piece_factory = BoardPieceDefaultFactory()
        self._capture_box = CaptureBox(self._font, state.player)

        # create observers for controller
        self._move_piece_observers = []
        self._place_piece_observers = []
        self._receive_message_observers = []

        self.init_state(state)

    def init_state(self, state: GameState):
        # state variables
        self._current_player = state.player_to_move
        self._player = state.player
        self._turn = state.turn
        self._action = state.move
        self._game_status = state.game_status

        # grid initialization comes after player initialization
        self._grid = Grid(
            self._width,
            self._height,
            state.board.columns,
            state.board.rows,
            self._player,
        )
        self._setup_position(state)

        # mouse functionality variables
        self._active_cell_to_snap = None

    def _setup_position(self, state: GameState) -> None:
        pieces: Mapping[Location, PieceData] = state.board.pieces

        for location, piece_data in pieces.items():
            self._pieces[location] = self._piece_factory.make(
                piece_data.piece_kind,
                self._grid.get_position_from_location(location),
                self._grid.cell_length,
                piece_data.player,
                location,
                self._player,
            )

    # register move observer (usually from controller)
    def register_move_piece_observer(self, observer: MovePieceObserver) -> None:
        self._move_piece_observers.append(observer)

    def register_place_piece_observer(self, observer: PlacePieceObserver) -> None:
        self._place_piece_observers.append(observer)

    def register_receive_message_observer(
        self, observer: ReceiveMessageObserver
    ) -> None:
        self._receive_message_observers.append(observer)

    # --- MARK: Run PyGame
    def run(self, networking: CS150241ProjectNetworking | None) -> NoReturn:
        active_piece_index: Location | None = None
        active_capture_piece_index: int | None = None

        piece_to_check: Piece | None

        is_running: bool = True

        while is_running:
            if networking is not None:
                latest_message: Message | None = None
                for m in networking.recv():
                    latest_message = m

                if latest_message is not None:
                    for observer in self._receive_message_observers:
                        observer.on_receive_message(latest_message)

            for event in pygame.event.get():
                match (event.type, self._game_status):
                    case (pygame.QUIT, _):
                        pygame.quit()
                        sys.exit()

                    case (pygame.MOUSEBUTTONDOWN, GameStatus.ONGOING):
                        match event.button:
                            case 1:  # left mouse button
                                # check if click is for Board
                                for loc in self._pieces.keys():
                                    piece_to_check = self._pieces[loc]
                                    if (
                                        piece_to_check.rect.collidepoint(event.pos)
                                        and piece_to_check.owned_by == self._player
                                    ):
                                        active_piece_index = loc
                                        break

                                # check if click is for Capture List: prev and next button
                                for index_b, button in enumerate(
                                    self._capture_box.buttons
                                ):
                                    if button.collision_box.collidepoint(event.pos):
                                        self._capture_box.go_to_page(index_b)

                                # check if click is for Capture List
                                # Note: the loop will only iterate through the shown capture list (maximum of 4)
                                for index_c, cap_piece in enumerate(
                                    self._capture_box.shown_capture_list,
                                    self._capture_box.start_of_slice,
                                ):
                                    if cap_piece.rect.collidepoint(event.pos):
                                        active_capture_piece_index = index_c
                                        break

                            case 3:
                                if active_piece_index is not None:
                                    for loc in self._pieces.keys():
                                        piece_to_check = self._pieces[loc]
                                        if active_piece_index == loc:
                                            piece_to_check.reset_to_spot()
                                            break
                                    # point active piece index to nothing
                                    active_piece_index = None

                                if active_capture_piece_index is not None:
                                    for index_c, cap_piece in enumerate(
                                        self._capture_box.capture_list
                                    ):
                                        if active_capture_piece_index == index_c:
                                            cap_piece.reset_to_spot()
                                            break
                                    active_capture_piece_index = None

                            case _:
                                pass

                    case (pygame.MOUSEBUTTONUP, GameStatus.ONGOING):
                        # check which cell to snap to.
                        # note that the position should only snap to one cell (if not, we are in some big trouble)
                        match event.button:
                            case 1:
                                if active_piece_index is not None:
                                    piece_to_check = self._pieces[active_piece_index]
                                    old_cell_location: Location = (
                                        self._grid.get_location_from_position(
                                            piece_to_check.last_stable_position
                                        )
                                    )
                                    snap_cell: Rect | None = self._grid.snap_position(
                                        event.pos
                                    )

                                    if snap_cell is None:
                                        piece_to_check.reset_to_spot()
                                    else:
                                        new_cell_location: Location = (
                                            self._grid.get_location_from_cell(snap_cell)
                                        )

                                        self._active_cell_to_snap = snap_cell
                                        self._move_piece(
                                            old_cell_location,
                                            new_cell_location,
                                            self._player,
                                        )

                                    # point active piece index to nothing
                                    self._active_cell_to_snap = None
                                    active_piece_index = None

                                if active_capture_piece_index is not None:
                                    cap_piece: CapturedPiece = (
                                        self._capture_box.capture_list[
                                            active_capture_piece_index
                                        ]
                                    )
                                    snap_cell: Rect | None = self._grid.snap_position(
                                        event.pos
                                    )

                                    if snap_cell is None:
                                        self._capture_box.reset_captured_pieces()
                                    else:
                                        new_cell_location: Location = (
                                            self._grid.get_location_from_cell(snap_cell)
                                        )
                                        self._active_cell_to_snap = snap_cell
                                        self._place_piece(
                                            cap_piece.piece_kind,
                                            new_cell_location,
                                            self._player,
                                        )

                                    active_capture_piece_index = None

                            case _:
                                pass

                    case (pygame.MOUSEMOTION, GameStatus.ONGOING):
                        if active_piece_index is not None:
                            self._pieces[active_piece_index].move_rel(event.rel)

                        if active_capture_piece_index is not None:
                            self._capture_box.move_piece(
                                event.rel, active_capture_piece_index
                            )

                    case (_, _):
                        pass

            # render all assets
            self.render_frame()

            pygame.display.flip()

            self._clock.tick(self._fps)
            self._frame_count += 1

    def render_frame(self) -> None:
        self._screen.fill('black')

        self._render_game_details()

        self._grid.render(self._screen)

        group_sprites: Group[Sprite] = Group()

        for loc in self._pieces.keys():
            self._pieces[loc].update_position()
            group_sprites.add(self._pieces[loc])

        group_sprites.draw(self._screen)

        self._capture_box.render(self._screen, self._font)

        # when game has ended, output who won (or drew)
        if self._game_status == GameStatus.DRAW:
            self._render_end_screen('Draw')
        elif self._game_status == GameStatus.PLAYER_1_WIN:
            self._render_end_screen('Player 1 (Orange) Wins')
        elif self._game_status == GameStatus.PLAYER_2_WIN:
            self._render_end_screen('Player 2 (Blue) Wins')

        return

    def _render_game_details(self) -> None:
        # current player (and client player)
        player_render: Surface = self._font.render(
            f'Current Player: {self._current_player} ({"You" if self._current_player == self._player else "Opponent" })',
            True,
            'white',
        )
        player_rect: Rect = player_render.get_rect()
        player_rect.center = (
            (self._width - (self._width * REL_CAPTURED_BOX_WIDTH)) // 2,
            REL_TEXT_MARGIN,
        )
        self._screen.blit(player_render, player_rect)

        turn_render: Surface = self._font.render(
            f'Turn {self._turn}, Action {self._action}', True, 'white'
        )
        turn_rect: Rect = turn_render.get_rect()
        turn_rect.center = (
            (self._width - (self._width * REL_CAPTURED_BOX_WIDTH)) // 2,
            SCREEN_HEIGHT - REL_TEXT_MARGIN,
        )
        self._screen.blit(turn_render, turn_rect)

    def _render_end_screen(self, msg: str) -> None:
        # translucent frame
        frame: Surface = pygame.Surface((SCREEN_WIDTH, SCREEN_HEIGHT))
        frame.set_alpha(200)
        frame.fill((0, 0, 0))
        self._screen.blit(frame, (0, 0))

        # message
        status: Surface = self._font.render(msg, True, 'white')
        status_rect: Rect = status.get_rect()
        status_rect.center = (
            (self._width // 2),
            (self._height // 2),
        )
        self._screen.blit(status, status_rect)

        return

    # --- MARK: Piece Movements
    def _move_piece(self, old: Location, new: Location, player: Player) -> None:
        for observer in self._move_piece_observers:
            observer.on_move_piece(old, new, player)

    def _place_piece(
        self,
        piece_kind: PieceKind,
        dest: Location,
        player: Player,
    ) -> None:
        for observer in self._place_piece_observers:
            observer.on_place_piece(piece_kind, dest, player)

    def update_move(self, fb: MoveFeedback) -> None:
        active_piece: Piece = self._pieces[fb.move_src]
        match fb.info:
            case MoveFeedbackInfo.VALID:
                if fb.move_dest is None:
                    raise RuntimeError('Error: Move destination was not found')

                self._active_cell_to_snap = self._grid.get_cell_from_location(
                    fb.move_dest
                )

                # remove piece from prev location
                self._pieces.pop(fb.move_src)

                # set piece to new location
                self._pieces[fb.move_dest] = active_piece

                # snap to location
                # if self._active_cell_to_snap is not None:
                self._pieces[fb.move_dest].snap(self._active_cell_to_snap)

            case _:
                self._pieces[fb.move_src].reset_to_spot()

    def update_place(self, fb: PlaceFeedback) -> None:
        match fb.info:
            case PlaceFeedbackInfo.VALID:
                if fb.place_dest is None:
                    raise RuntimeError('Error: Place destination was not found')

                # add new piece in location
                self._pieces[fb.place_dest] = self._piece_factory.make(
                    fb.place_piece_kind,
                    self._grid.get_position_from_location(fb.place_dest),
                    self._grid.cell_length,
                    self._current_player,
                    fb.place_dest,
                    self._player,
                )
            case _:
                self._capture_box.reset_captured_pieces()
        pass

    def on_state_change(self, state: GameState) -> None:
        self._current_player = state.player_to_move
        self._capture_box.update_captured_list(state.captured_pieces, self._player)
        self._turn = state.turn
        self._action = state.move
        self._game_status = state.game_status
