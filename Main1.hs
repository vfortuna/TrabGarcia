{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, EmptyDataDecls, ViewPatterns #-}
module Main where
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Control.Applicative
import Yesod
import Yesod.Form.Jquery
import Yesod.Static
-- 1
data Pagina = Pagina{connPool :: ConnectionPool,
                     getStatic :: Static 
                    }
-- 2
instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Livro
   nome Text
   autor Text
   editora Text
   ano Day 
   tipo Text
   deriving Show

Aluno
   nome Text 
   cpf Int
   curso Text

Emprestimo
   livroId LivroId
   alunoId AlunoId
   data UTCTime default=now()
   processado Bool
   --UniqueFornPeca fornId pecaId
|]
-- 3
staticFiles "static"
-- 4
mkYesod "Pagina" [parseRoutes|
  /livro LivroR GET POST
  /aluno AlunoR GET POST
  /listalivro ListarLivroR GET
  /listaaluno ListarAlunoR GET
  /emprestimo EmprestimoR GET POST
  / HomeR GET
  /static StaticR Static getStatic
  /listaEmpreestimo ListarEmprestimoR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance YesodJquery Pagina where

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage

formEmprestimo :: Form Emprestimo
formEmprestimo = renderDivs $ Emprestimo <$>
             areq (selectField livro) "Livro" Nothing <*>
             areq (selectField aluno) "Aluno" Nothing <*>
             lift (liftIO getCurrentTime) <*>
             lift (liftIO $ return False)

livro = do
       entidades <- runDB $ selectList [] [Asc LivroNome] 
       optionsPairs $ fmap (\ent -> (livroNome $ entityVal ent, entityKey ent)) entidades

aluno = do
       entidades <- runDB $ selectList [] [Asc AlunoNome] 
       optionsPairs $ fmap (\ent -> (alunoNome $ entityVal ent, entityKey ent)) entidades

formLivro :: Form Livro
formLivro = renderDivs $ Livro <$>
             areq textField "Nome:" Nothing <*>
             areq textField "Autor:" Nothing <*>
             areq textField "Editora:" Nothing <*>
             areq (jqueryDayField def { jdsChangeYear = True 
                 , jdsYearRange = "1980:2017"}) "Ano" Nothing <*>
             areq textField "Tipo" Nothing

formAluno :: Form Aluno
formAluno = renderDivs $ Aluno <$>
             areq textField "Nome" Nothing <*>
             areq intField "CPF" Nothing <*>
             areq textField "Curso" Nothing  

widgetForm :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <h1>
                Cadastro de #{y}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
|]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
           addStylesheet $ StaticR teste_css
           [whamlet|
               <img src=@{StaticR engine_jpg}> 
               <ul>
                  <li> <a href=@{LivroR}> Cadastro de livro
                  <li> <a href=@{AlunoR}> Cadastro de aluno
                  <li> <a href=@{EmprestimoR}> Cadastro de livros emprestados
                  <li> <a href=@{ListarLivroR}> Listagem de livro
                  <li> <a href=@{ListarAlunoR}> Listagem de aluno
                  <li> <a href=@{ListarEmprestimoR}> Listagem de livros emprestados
               
           |]

getLivroR :: Handler Html
getLivroR = do
           (widget, enctype) <- generateFormPost formLivro
           defaultLayout $ widgetForm LivroR enctype widget "Livro"

postLivroR :: Handler Html
postLivroR = do
            ((result,_),_) <- runFormPost formLivro
            case result of
                FormSuccess livro -> (runDB $ insert livro) >> defaultLayout [whamlet|<h1> Livro cadastrado com sucesso!|]
                _ -> redirect LivroR


getAlunoR :: Handler Html
getAlunoR = do
           (widget, enctype) <- generateFormPost formAluno
           defaultLayout $ widgetForm AlunoR enctype widget "Alunos"

postAlunoR :: Handler Html
postAlunoR = do
            ((result,_),_) <- runFormPost formAluno
            case result of
                FormSuccess aluno -> (runDB $ insert aluno) >> defaultLayout [whamlet|<h1> Aluno cadastrado com sucesso!|]
                _ -> redirect AlunoR

getListarLivroR :: Handler Html
getListarLivroR = do
                 livro <- runDB $ selectList [] [Asc LivroNome]
                 defaultLayout [whamlet|
                      <h1> Lista de Livros
                      $forall Entity _ lent <- livro
                          <h2> 
                              #{livroNome lent}
                              
                 |]

getListarAlunoR :: Handler Html
getListarAlunoR = do
                 aluno <- runDB $ selectList [] [Asc AlunoNome]
                 defaultLayout [whamlet|
                      <h1> Lista de Alunos
                      $forall Entity aid aent <- aluno
                          <h2> #{alunoNome aent}
                 |]

getEmprestimoR :: Handler Html
getEmprestimoR = do
           (widget, enctype) <- generateFormPost formEmprestimo
           defaultLayout $ widgetForm EmprestimoR enctype widget "Emprestimo"

postEmprestimoR :: Handler Html
postEmprestimoR = do
            ((result,_),_) <- runFormPost formEmprestimo
            case result of
                FormSuccess x -> (runDB $ insert x) >> defaultLayout [whamlet|<h1> Emprestimo inserido|]
                _ -> redirect EmprestimoR

getListarEmprestimoR :: Handler Html
getListarEmprestimoR = do
                 emprestimo <- runDB $ (rawSql "SELECT ??, ?? \
                                   \FROM emprestimo INNER JOIN livro \
                                   \ON emprestimo.livro_id=livro.id" [])::Handler [(Entity Emprestimo, Entity Livro)]
                 defaultLayout [whamlet|
                      <h1> Lista de Emprestimos
                      $forall (Entity oq emprestimo, Entity _ np) <- emprestimo
                          <p> Emprestimo do dia #{show $ utctDay $ emprestimoData emprestimo} #{fromSqlKey oq}: #{livroNome np}
                 |]


connStr = "dbname=d8vp0kbbtr8qgb host=ec2-23-21-165-201.compute-1.amazonaws.com user=zhdycdxhtyakmn password=1J-RncKEkrJg1YpAfmkrFJen4Q port=5432"
-- 5

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       t@(Static settings) <- static "static"
       warp 8080 (Pagina pool t)
