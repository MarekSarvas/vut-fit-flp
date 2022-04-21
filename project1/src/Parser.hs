-- Project: FLP project 1 - BKG-2-CNF
-- Author: Marek Sarvas
-- Login: xsarva00
-- Date: 2021/2022
-- Module: Parsing grammar into inner representation

module Parser (parseGrammar) where 

import Control.Applicative
import Text.Parsec (char, endBy, eof, many1, newline, oneOf, parse, ParseError, sepBy1, string)
import Text.Parsec.String (Parser)

import Types (Nonterminals, Nonterminal, Terminal, Terminals, Rule, Rules, Grammar(..))

-- Parse correct Nonterminal input 
parseNonTerminals :: Parser Nonterminals 
parseNonTerminals = sepBy1 parseOneNonTerminal (char ',') <* newline --read until \n

parseOneNonTerminal :: Parser Nonterminal 
parseOneNonTerminal = fmap (\x -> [x]) (oneOf ['A'..'Z']) 

-- Parse correct Terminal input 
parseTerminals :: Parser Terminals 
parseTerminals = sepBy1 parseOneTerminal (char ',') <* newline --read until \n 

parseOneTerminal :: Parser Terminal 
parseOneTerminal = fmap (\x -> [x]) (oneOf ['a'..'z']) 

-- parse starting symbol
parseStartSymbol :: Parser String 
parseStartSymbol =  fmap (\x -> [x]) (oneOf ['A'..'Z']) <* newline

-- parse rules 
parseRules :: Parser Rules
parseRules =  endBy parseRule newline 

parseRule :: Parser Rule 
parseRule = toRule <$> parseLeft <*> parseRight
    where toRule l r = (l, map (\x -> [x]) r)
 
-- parse left side of the rule
parseLeft :: Parser String 
parseLeft = fmap (: []) (oneOf ['A'..'Z']) <* string "->" 

-- parse right side od the rule
parseRight :: Parser String 
parseRight = many1 (oneOf (['A'..'Z']++['a'..'z']))

-- parse whole content to Grammar inner representation
parseGrammar :: String -> Either ParseError Grammar
parseGrammar = parse parseGrammar'' "Wrong input grammar"

-- call all parser and create Grammar representaion
parseGrammar'' :: Parser Grammar 
parseGrammar''  = Grammar <$> parseNonTerminals <*> parseTerminals <*> parseStartSymbol <*> parseRules <* eof
