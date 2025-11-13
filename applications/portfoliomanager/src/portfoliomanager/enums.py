from enum import Enum


class PositionAction(Enum):
    PDT_LOCKED = "PDT_LOCKED"
    CLOSE_POSITION = "CLOSE_POSITION"
    MAINTAIN_POSITION = "MAINTAIN_POSITION"
    UNSPECIFIED = "UNSPECIFIED"


class TradeSide(Enum):
    BUY = "BUY"
    SELL = "SELL"


class PositionSide(Enum):
    LONG = "LONG"
    SHORT = "SHORT"
