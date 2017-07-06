module Parser where
    
    import Control.Applicative
    
    import qualified Text.Parsec as Parsec
    
    import Syntax
    
    -- basic 'terminal' tokens

    atom :: Parsec.Parsec String () String
    atom = Parsec.many1 Parsec.letter
    
    mapsTo :: Parsec.Parsec String () ()
    mapsTo = do Parsec.string "->" <|> Parsec.string ","; return ()

    open :: Parsec.Parsec String () ()
    open = do Parsec.string "("; return ()
    
    close :: Parsec.Parsec String () ()
    close = do Parsec.string ")"; return ()
    
    -- meta functions
    
    addParens :: Parsec.Parsec String () a -> Parsec.Parsec String () a
    addParens parser = do
        open
        Parsec.spaces
        parsed <- parser
        Parsec.spaces
        close
        return parsed
    
    tryChoices :: [Parsec.Parsec String () a] -> Parsec.Parsec String () a
    tryChoices ls = Parsec.choice $ map Parsec.try ls
    
    spacedString :: String -> Parsec.Parsec String () String
    spacedString str = do
        Parsec.spaces
        sval <- Parsec.string str
        Parsec.spaces
        return sval
        
    spacedExpr :: Parsec.Parsec String () a -> Parsec.Parsec String () a
    spacedExpr parser = do
        Parsec.spaces
        val <- parser
        Parsec.spaces
        return val
    
    -- expression parsers

    atomExpr :: Parsec.Parsec String () Expression
    atomExpr = do
        name <- atom
        return (Atomic name)

    lambda :: Parsec.Parsec String () Expression
    lambda = do
        free <- atom
        spacedExpr mapsTo
        body <- tryChoices [lambda, application, atomExpr, parenExpr]
        return (Function free body)
    
    argument :: Parsec.Parsec String () Expression
    argument = tryChoices [parenExpr, atomExpr]
    
    application_ :: Expression -> Parsec.Parsec String () (Maybe Expression)
    application_ function = Parsec.optionMaybe $ do
        arg <- argument
        let func = (Apply function arg)
        Parsec.spaces
        app <- application_ func
        return $ case app of
            Nothing   -> func
            Just app  -> app

    application :: Parsec.Parsec String () Expression
    application = do
        func <- tryChoices [parenExpr, atomExpr]
        Parsec.spaces
        app <- application_ func
        return $ case app of
            Nothing  -> func
            Just app -> app
            
    parenExpr :: Parsec.Parsec String () Expression
    parenExpr = do
        open
        value <- spacedExpr start
        close
        return value
    
    letExpr :: Parsec.Parsec String () Expression
    letExpr = do
        Parsec.string "let"
        Parsec.spaces
        var <- atom
        spacedString "="
        val <- start
        spacedString ";"
        rest <- start
        return (Apply (Function var rest) val)
    
    comment :: Parsec.Parsec String () Expression
    comment = do
        Parsec.char '#'
        Parsec.many $ Parsec.noneOf "\n"
        Parsec.char '\n'
        rest <- start
        return rest
    
    start :: Parsec.Parsec String () Expression
    start = tryChoices [comment, letExpr, lambda, application, parenExpr]

    -- convenience parsing function

    parseHelper :: Parsec.Parsec String () a -> String -> Either Parsec.ParseError a
    parseHelper rule text = Parsec.parse rule "(source)" text
    
    parse :: String -> Either Parsec.ParseError Expression
    parse text = parseHelper start text
