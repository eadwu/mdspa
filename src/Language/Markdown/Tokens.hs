module Language.Markdown.Tokens
( MarkdownToken(..), JSONToken(..) )
where
  data HTMLSupport
    = HTMLText String -- ^ Well, mainly for completeness sake, and *cough* alignment
    | HTMLTagOpen String | HTMLTagClose String -- ^ Since HTML doesn't need to be parsed, this should be good enough
    -- Well not quite, since HTML can get quite messy, but assume this is the *ideal* HTML
    deriving (Show, Eq)

  data MarkdownSupport
    = Comment String
    -- Layout
    | Header Int String -- ^ <h#>, # in [1..7]
    | Paragraph String -- ^ <p>
    -- Text specific
    | BoldText String | ItalicText String | Text String
    deriving (Show, Eq)

  data MarkdownToken = MarkdownSupport | HTMLSupport

  data JSONToken
    = JSONString String
    | JSONBool Bool
    | JSONInt Integer
    deriving (Show, Eq)
