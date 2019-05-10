module Text.Mining.Sentiment.Emotion where

-- | Circumplex of emotions (Plutchik, 1980)
data Emotion
    = Anticipation
    | Joy
    | Trust
    | Fear
    | Sadness
    | Disgust
    | Anger
    | Surprise
    | Positive
    | Negative
    | None
    deriving (Show, Eq, Ord, Enum)
