from __future__ import annotations
from collections.abc import Mapping
from dataclasses import dataclass
from enum import StrEnum
from typing import Protocol, TypedDict

# --- MARK: GameStatus


class GameStatus(StrEnum):
    ONGOING = 'Ongoing'
    DRAW = 'Draw'
    PLAYER_1_WIN = 'Player 1 wins'
    PLAYER_2_WIN = 'Player 2 wins'


# --- MARK: GameState


# basically a ReadOnly(Model+State)
class GameState(Protocol):
    @property
    def player(self) -> Player: ...

    @property
    def max_moves(self) -> int: ...

    @property
    def board(self) -> BoardData: ...

    @property
    def captured_pieces(self) -> Mapping[Player, Mapping[PieceKind, int]]: ...

    @property
    def player_to_move(self) -> Player: ...

    @property
    def turn(self) -> int: ...

    @property
    def move(self) -> int: ...

    @property
    def game_status(self) -> GameStatus: ...


# --- MARK: Player


class Player(StrEnum):
    PLAYER_1 = 'Player 1'
    PLAYER_2 = 'Player 2'


# --- MARK: PieceKind


class PieceKind(StrEnum):
    PAWN = 'Pawn'
    GRAIL = 'Grail'
    LANCE = 'Lance'
    FLAG_LEFT = 'FlagLeft'
    FLAG_RIGHT = 'FlagRight'
    SWORD = 'Sword'
    BOW = 'Bow'
    DAGGER = 'Dagger'


# --- MARK: Location


@dataclass(frozen=True)
class Location:
    _row: int
    _column: int

    def __add__(self, other: Location | tuple[int, int]) -> Location:
        """Add value to a `Location()` instance. Value can be another `Location()` or a `tuple[int, int].`"""
        if isinstance(other, Location):
            return Location(self._row + other.row, self._column + other.column)
        elif type(other) is tuple[int, int]:
            return Location(self._row + other[0], self._column + other[1])
        else:
            raise TypeError('Error: Invalid type for __add__ with Location().')

    def __copy__(self) -> Location:
        """Return a new `Location()` instance with the same values."""
        return Location(self._row, self._column)

    def __hash__(self) -> int:
        """Hash a `Location()` instance by hashing a `tuple[int, int]` that contains the values of `.row` and `.column`."""
        return hash((self._row, self._column))

    def __rsub__(self, other: Location | tuple[int, int]) -> Location:
        """Subtract value from a `Location()` instance. Value can be another `Location()` or a `tuple[int, int].`"""
        if isinstance(other, Location):
            return Location(other._row - self.row, other._column - self.column)
        elif type(other) is tuple[int, int]:
            return Location(other[0] - self._row, other[1] - self._column)
        else:
            raise TypeError('Error: Invalid type for __rsub__ with Location().')

    def __sub__(self, other: Location | tuple[int, int]) -> Location:
        """Decrease value by a `Location()` instance. Value can be another `Location()` or a `tuple[int, int].`"""
        if isinstance(other, Location):
            return Location(self._row - other.row, self._column - other.column)
        elif type(other) is tuple[int, int]:
            return Location(self._row - other[0], self._column - other[1])
        else:
            raise TypeError('Error: Invalid type for __sub__ with Location().')

    @property
    def row(self) -> int:
        """Returns the row number of a `Location()` instance (1-indexed)."""
        return self._row

    @property
    def column(self) -> int:
        """Returns the column number of a `Location()` instance (1-indexed)."""
        return self._column


# --- MARK: PieceData


class PieceData(Protocol):
    @property
    def player(self) -> Player: ...

    @property
    def piece_kind(self) -> PieceKind: ...

    @property
    def location(self) -> Location: ...


# --- MARK: BoardData


class BoardData(Protocol):
    @property
    def rows(self) -> int: ...

    @property
    def columns(self) -> int: ...

    @property
    def pieces(self) -> Mapping[Location, PieceData]: ...


# --- MARK: MoveFeedbackInfo


class MoveFeedbackInfo(StrEnum):
    NOT_CURRENT_PLAYER = 'Not current player'
    SQUARE_OUT_OF_BOUNDS = 'Square out of bounds'
    NO_PIECE_MOVED = 'No piece moved'
    PIECE_DOES_NOT_BELONG_TO_PLAYER = 'Piece does not belong to player'
    PIECE_CANNOT_REACH_SQUARE = 'Piece cannot reach square'
    CAPTURES_OWN_PIECE = 'Captures own piece'
    CAPTURES_PROTECTED_PIECE = 'Captures protected piece'
    PIECE_CANNOT_CAPTURE = 'Piece cannot capture'
    VALID = 'Valid'


@dataclass(frozen=True)
class MoveFeedback:
    move_src: Location
    move_dest: Location | None
    info: MoveFeedbackInfo


# --- MARK: PlaceFeedbackInfo


class PlaceFeedbackInfo(StrEnum):
    NOT_CURRENT_PLAYER = 'Not current player'
    SQUARE_OUT_OF_BOUNDS = 'Square out of bounds'
    NO_PLAYER_PLAYED = 'No player played'
    NO_PIECE_PLACED = 'No piece placed'
    BLOCKS_PROTECTED_PIECE_MOVEMENT = 'Blocks protected piece movement'
    CAPTURES_PIECE = 'Captures a piece'
    VALID = 'Valid'


@dataclass(frozen=True)
class PlaceFeedback:
    place_piece_kind: PieceKind
    place_dest: Location | None
    info: PlaceFeedbackInfo


# --- MARK: GameMessageDict


class GameMessageDict(TypedDict, total=True):
    message_type: GameMessageType
    message_content: GameMessageContentDict


# --- MARK: GameMessageType


class GameMessageType(StrEnum):
    MOVE = 'move'
    PLACE = 'place'
    INVALID = 'invalid'

    @classmethod
    def _missing_(cls, value: object) -> GameMessageType:
        return GameMessageType.INVALID


# --- MARK: GameMessageContentDict


class LocationDict(TypedDict):
    row: int
    col: int


class GameMessageContentDict(TypedDict, total=False):
    move_src: LocationDict
    move_dest: LocationDict
    place_piece_kind: PieceKind
    place_dest: LocationDict


class MakeMoveGameMessageContentDict(GameMessageContentDict, total=False):
    move_src: LocationDict
    move_dest: LocationDict


class PlacePieceGameMessageContentDict(GameMessageContentDict, total=False):
    place_piece_kind: PieceKind
    place_dest: LocationDict
