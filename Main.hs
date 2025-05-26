-- UseCaseDiagramInteractive.hs
-- Interactive Use Case Diagram Generator in Haskell
-- This program defines data structures for use case diagrams and functions
-- to add elements, validate relationships, and generate PlantUML code.

import Data.List (isInfixOf, nub, find)
import System.IO (writeFile, IOMode(WriteMode), hClose, openFile, hPutStrLn, stdout, hFlush)
import Data.Char (isAlphaNum, toLower, isDigit)
import Text.Read (readMaybe) -- For safer number parsing

-- ==================================================
-- Data Type Definitions
-- ==================================================

type ElementName = String
data Actor = Actor ElementName deriving (Show, Eq, Ord)
data UseCase = UseCase ElementName deriving (Show, Eq, Ord)
type SystemName = ElementName

data Relationship
    = Association ElementName ElementName -- ActorName, UseCaseName
    | Include ElementName ElementName     -- BaseUCName, IncludedUCName
    | Extend ElementName ElementName      -- ExtendingUCName, ExtendedUCName
    | GeneralizationUseCase ElementName ElementName -- ChildUCName, ParentUCName
    | GeneralizationActor ElementName ElementName   -- ChildActorName, ParentActorName
    deriving (Eq)

-- Custom Show instance for Relationship to make listings prettier
instance Show Relationship where
    show (Association a uc)         = "'" ++ a ++ "' -- '" ++ uc ++ "'"
    show (Include base inc)         = "'" ++ base ++ "' ..> '" ++ inc ++ "' : <<include>>"
    show (Extend ext base)          = "'" ++ ext ++ "' ..> '" ++ base ++ "' : <<extend>>"
    show (GeneralizationUseCase c p)= "'" ++ c ++ "' --|> '" ++ p ++ "'"
    show (GeneralizationActor c p)  = "'" ++ c ++ "' --|> '" ++ p ++ "'"

data UseCaseDiagram = UseCaseDiagram {
    systemName    :: Maybe SystemName,
    actors        :: [Actor],
    useCases      :: [UseCase],
    relationships :: [Relationship]
} deriving (Show, Eq)

initialDiagram :: UseCaseDiagram
initialDiagram = UseCaseDiagram Nothing [] [] []

-- ==================================================
-- Helper Functions for Element Checking (Pure)
-- ==================================================
actorExists :: ElementName -> UseCaseDiagram -> Bool
actorExists name diagram = Actor name `elem` actors diagram

useCaseExists :: ElementName -> UseCaseDiagram -> Bool
useCaseExists name diagram = UseCase name `elem` useCases diagram

relationshipExists :: Relationship -> UseCaseDiagram -> Bool
relationshipExists rel diagram = rel `elem` relationships diagram

-- ==================================================
-- Functions for "Inserting" Specifications (Pure)
-- ==================================================
addActor :: ElementName -> UseCaseDiagram -> Maybe UseCaseDiagram
addActor name diagram
    | null name || actorExists name diagram = Nothing
    | otherwise = Just diagram { actors = nub (Actor name : actors diagram) }

addUseCase :: ElementName -> UseCaseDiagram -> Maybe UseCaseDiagram
addUseCase name diagram
    | null name || useCaseExists name diagram = Nothing
    | otherwise = Just diagram { useCases = nub (UseCase name : useCases diagram) }

setSystemNamePure :: SystemName -> UseCaseDiagram -> UseCaseDiagram
setSystemNamePure name diagram
    | null name = diagram 
    | otherwise = diagram { systemName = Just name }

addRelationshipPure :: Relationship -> UseCaseDiagram -> Maybe UseCaseDiagram
addRelationshipPure rel diagram
    | relationshipExists rel diagram = Nothing
    | not (validateRelationship rel diagram) = Nothing
    | otherwise = Just diagram { relationships = nub (rel : relationships diagram) }

-- Validates relationships. Crucially, for any relation involving use cases,
-- it verifies if those use cases are already defined, as per requirement #2.
-- For completeness and practical diagram validity, it also checks actors
-- when they are part of the relation (e.g., Associations, Actor Generalizations).
validateRelationship :: Relationship -> UseCaseDiagram -> Bool
validateRelationship (Association actorName ucName) diagram =
    useCaseExists ucName diagram && actorExists actorName diagram
validateRelationship (Include baseUCName includedUCName) diagram =
    useCaseExists baseUCName diagram && useCaseExists includedUCName diagram && baseUCName /= includedUCName
validateRelationship (Extend extendingUCName extendedUCName) diagram =
    useCaseExists extendingUCName diagram && useCaseExists extendedUCName diagram && extendingUCName /= extendedUCName
validateRelationship (GeneralizationUseCase childUCName parentUCName) diagram =
    useCaseExists childUCName diagram && useCaseExists parentUCName diagram && childUCName /= parentUCName
validateRelationship (GeneralizationActor childActorName parentActorName) diagram =
    actorExists childActorName diagram && actorExists parentActorName diagram && childActorName /= parentActorName

-- ==================================================
-- PlantUML Code Generation (Pure)
-- ==================================================
sanitizeName :: ElementName -> String
sanitizeName name =
    let lowerName = map toLower name
        noSpacesOrHyphens = map (\c -> if c == ' ' || c == '-' then '_' else c) lowerName
        alphaNumUnderscoreOnly = filter (\c -> isAlphaNum c || c == '_') noSpacesOrHyphens
        validId = if null alphaNumUnderscoreOnly then "unspecified_id" else alphaNumUnderscoreOnly
    in if not (null validId) && isDigit (head validId)
           then "id_" ++ validId
           else validId

actorToPlantUML :: Actor -> String
actorToPlantUML (Actor name) = "actor \"" ++ name ++ "\" as " ++ sanitizeName name

useCaseToPlantUML :: UseCase -> String
useCaseToPlantUML (UseCase name) = "usecase \"" ++ name ++ "\" as " ++ sanitizeName name

relationshipToPlantUML :: Relationship -> String
relationshipToPlantUML rel = case rel of
    Association actorName ucName -> sanitizeName actorName ++ " -- " ++ sanitizeName ucName
    Include baseUC includedUC -> sanitizeName baseUC ++ " ..> " ++ sanitizeName includedUC ++ " : <<include>>"
    Extend extendingUC extendedUC -> sanitizeName extendingUC ++ " ..> " ++ sanitizeName extendedUC ++ " : <<extend>>"
    GeneralizationUseCase childUC parentUC -> sanitizeName childUC ++ " --|> " ++ sanitizeName parentUC
    GeneralizationActor childActor parentActor -> sanitizeName childActor ++ " --|> " ++ sanitizeName parentActor

diagramToPlantUML :: UseCaseDiagram -> String
diagramToPlantUML diagram =
    unlines $
    ["@startuml"] ++
    titleLine ++
    ["left to right direction", "skinparam packageStyle rectangle", "skinparam actorStyle awesome", ""] ++
    map actorToPlantUML (actors diagram) ++ [""] ++
    packageStart ++
    map useCaseToPlantUML (useCases diagram) ++
    packageEnd ++ [""] ++
    map relationshipToPlantUML (relationships diagram) ++
    ["@enduml"]
  where
    titleLine = case systemName diagram of
              Just sn | not (null sn) -> ["title Use Case Diagram for " ++ sn, ""]
              _                     -> ["title Use Case Diagram", ""]
    packageStart = case systemName diagram of
                     Just sn | not (null sn) -> ["rectangle \"<<System>>\\n" ++ sn ++ "\" as " ++ sanitizeName sn ++ "_boundary {", ""]
                     _                     -> []
    packageEnd = case systemName diagram of
                   Just sn | not (null sn) -> ["", "}"]
                   _                     -> []

-- ==================================================
-- IO Helper Functions
-- ==================================================

prompt :: String -> IO String
prompt message = do
    putStr (message ++ " ")
    hFlush stdout
    getLine

getChoice :: IO Int
getChoice = do
    choiceStr <- prompt "Choose an option:"
    case readMaybe choiceStr of
        Just val -> return val
        Nothing  -> do
            putStrLn "Invalid input. Please enter a number."
            getChoice

printIndexedList :: Show a => [a] -> IO ()
printIndexedList items =
    if null items
    then putStrLn "No items to display."
    else mapM_ putStrLn $ zipWith (\i item -> show i ++ ". " ++ show item) [1..] items

selectItem :: Show a => String -> [a] -> (a -> ElementName) -> IO (Maybe ElementName)
selectItem itemType items getNameFunc = do
    if null items
    then do
        putStrLn ("No " ++ itemType ++ "s available to select.")
        return Nothing
    else do
        putStrLn ("--- Available " ++ itemType ++ "s ---")
        printIndexedList items
        indexStr <- prompt ("Enter number of " ++ itemType ++ " to select (or 0 to cancel):")
        case readMaybe indexStr of
            Just idx
                | idx == 0 -> putStrLn "Cancelled." >> return Nothing
                | idx > 0 && idx <= length items -> return $ Just (getNameFunc (items !! (idx - 1)))
            _ -> putStrLn "Invalid selection." >> selectItem itemType items getNameFunc

getActorName :: Actor -> ElementName
getActorName (Actor name) = name

getUseCaseName :: UseCase -> ElementName
getUseCaseName (UseCase name) = name

-- ==================================================
-- IO Actions for Modifying Diagram
-- ==================================================
actionAddActor :: UseCaseDiagram -> IO UseCaseDiagram
actionAddActor diagram = do
    name <- prompt "Enter actor name:"
    if null name then do
        putStrLn "Actor name cannot be empty."
        return diagram
    else
        case addActor name diagram of
            Just newDiagram -> putStrLn ("Actor '" ++ name ++ "' added.") >> return newDiagram
            Nothing         -> putStrLn ("Actor '" ++ name ++ "' already exists or is invalid.") >> return diagram

actionAddUseCase :: UseCaseDiagram -> IO UseCaseDiagram
actionAddUseCase diagram = do
    name <- prompt "Enter use case name:"
    if null name then do
        putStrLn "Use case name cannot be empty."
        return diagram
    else
        case addUseCase name diagram of
            Just newDiagram -> putStrLn ("Use Case '" ++ name ++ "' added.") >> return newDiagram
            Nothing         -> putStrLn ("Use Case '" ++ name ++ "' already exists or is invalid.") >> return diagram

actionSetSystemName :: UseCaseDiagram -> IO UseCaseDiagram
actionSetSystemName diagram = do
    name <- prompt "Enter system name for the diagram (this will act as the main package):"
    if null name then do
        putStrLn "System name cannot be empty."
        return diagram
    else do
        let newDiagram = setSystemNamePure name diagram
        putStrLn ("System name set to '" ++ name ++ "'.")
        return newDiagram

actionAddRelationship :: (ElementName -> ElementName -> Relationship) -> String -> String -> String -> UseCaseDiagram -> IO UseCaseDiagram
actionAddRelationship relConstructor relType el1Type el2Type diagram = do
    putStrLn ("Select " ++ el1Type ++ " for " ++ relType ++ ":")
    maybeEl1Name <- if el1Type == "Actor"
                    then selectItem "Actor" (actors diagram) getActorName
                    else selectItem "Use Case" (useCases diagram) getUseCaseName
    case maybeEl1Name of
        Nothing -> return diagram
        Just el1Name -> do
            putStrLn ("Select " ++ el2Type ++ " for " ++ relType ++ " (related to '" ++ el1Name ++ "'):")
            maybeEl2Name <- if el2Type == "Actor"
                            then selectItem "Actor" (actors diagram) getActorName
                            else selectItem "Use Case" (useCases diagram) getUseCaseName
            case maybeEl2Name of
                Nothing -> return diagram
                Just el2Name -> do
                    let rel = relConstructor el1Name el2Name
                    case addRelationshipPure rel diagram of
                        Just newDiagram -> putStrLn (relType ++ " added: " ++ show rel) >> return newDiagram
                        Nothing         -> putStrLn ("Could not add " ++ relType ++ ". Invalid, duplicate, or self-referential.") >> return diagram

actionAddAssociationIO :: UseCaseDiagram -> IO UseCaseDiagram
actionAddAssociationIO = actionAddRelationship Association "Association" "Actor" "Use Case"

actionAddIncludeIO :: UseCaseDiagram -> IO UseCaseDiagram
actionAddIncludeIO = actionAddRelationship Include "<<include>> Relation" "Use Case" "Use Case"

actionAddExtendIO :: UseCaseDiagram -> IO UseCaseDiagram
actionAddExtendIO = actionAddRelationship Extend "<<extend>> Relation" "Use Case" "Use Case"

actionAddGenUCIO :: UseCaseDiagram -> IO UseCaseDiagram
actionAddGenUCIO = actionAddRelationship GeneralizationUseCase "Use Case Generalization" "Use Case" "Use Case"

actionAddGenActorIO :: UseCaseDiagram -> IO UseCaseDiagram
actionAddGenActorIO = actionAddRelationship GeneralizationActor "Actor Generalization" "Actor" "Actor"

actionGeneratePlantUML :: UseCaseDiagram -> IO ()
actionGeneratePlantUML diagram = do
    defaultFilename <- case systemName diagram of
                        Just sn | not (null sn) -> return $ sanitizeName sn ++ ".puml"
                        _                       -> return "use_case_diagram.puml"
    putStrLn $ "Default filename will be '" ++ defaultFilename ++ "'."
    filenameStr <- prompt "Enter filename (or press Enter for default, 0 to cancel):"
    case filenameStr of
        "0" -> putStrLn "Generation cancelled."
        ""  -> writePuml defaultFilename
        customFn -> writePuml (if ".puml" `isInfixOf` customFn then customFn else customFn ++ ".puml")
  where
    writePuml fn = do
        let plantUMLCode = diagramToPlantUML diagram
        handle <- openFile fn WriteMode
        System.IO.hPutStrLn handle plantUMLCode 
        hClose handle
        putStrLn ("Diagram saved to file '" ++ fn ++ "'")

-- ==================================================
-- Menu Loops
-- ==================================================
manageElementsLoop :: UseCaseDiagram -> IO UseCaseDiagram
manageElementsLoop diagram = do
    putStrLn ""
    putStrLn "--- Manage Elements ---"
    putStrLn "1. Add Actor"
    putStrLn "2. Add Use Case"
    putStrLn "0. Back to Main Menu"
    choice <- getChoice
    case choice of
        1 -> actionAddActor diagram >>= manageElementsLoop
        2 -> actionAddUseCase diagram >>= manageElementsLoop
        0 -> return diagram
        _ -> putStrLn "Invalid option." >> manageElementsLoop diagram

manageGeneralizationsLoop :: UseCaseDiagram -> IO UseCaseDiagram
manageGeneralizationsLoop diagram = do
    putStrLn ""
    putStrLn "--- Add Generalization ---"
    putStrLn "1. Actor Generalization"
    putStrLn "2. Use Case Generalization"
    putStrLn "0. Back to Manage Relationships Menu"
    choice <- getChoice
    case choice of
        1 -> actionAddGenActorIO diagram
        2 -> actionAddGenUCIO diagram
        0 -> return diagram
        _ -> putStrLn "Invalid option." >> manageGeneralizationsLoop diagram

manageRelationshipsLoop :: UseCaseDiagram -> IO UseCaseDiagram
manageRelationshipsLoop diagram = do
    putStrLn ""
    putStrLn "--- Manage Relationships ---"
    putStrLn "1. Add Association (Actor -- Use Case)"
    putStrLn "2. Add <<include>> Relation (Use Case ..> Use Case)"
    putStrLn "3. Add <<extend>> Relation (Use Case ..> Use Case)"
    putStrLn "4. Add Generalization"
    putStrLn "0. Back to Main Menu"
    choice <- getChoice
    case choice of
        1 -> actionAddAssociationIO diagram >>= manageRelationshipsLoop
        2 -> actionAddIncludeIO diagram >>= manageRelationshipsLoop
        3 -> actionAddExtendIO diagram >>= manageRelationshipsLoop
        4 -> manageGeneralizationsLoop diagram >>= manageRelationshipsLoop
        0 -> return diagram
        _ -> putStrLn "Invalid option." >> manageRelationshipsLoop diagram

mainMenuLoop :: UseCaseDiagram -> IO ()
mainMenuLoop diagram = do
    putStrLn ""
    case systemName diagram of
        Just sn | not (null sn) -> putStrLn ("Current System/Package: " ++ sn)
        _                       -> putStrLn "No system/package defined yet. Consider setting one (Option 3)."
    putStrLn "=== Main Menu ==="
    putStrLn "1. Manage Elements (Actors, Use Cases)"
    putStrLn "2. Manage Relationships"
    putStrLn "3. Set/Change System Name (Package)"
    putStrLn "4. Generate .puml Diagram File"
    putStrLn "0. Exit to Initial Menu"
    choice <- getChoice
    case choice of
        1 -> manageElementsLoop diagram >>= mainMenuLoop
        2 -> manageRelationshipsLoop diagram >>= mainMenuLoop
        3 -> actionSetSystemName diagram >>= mainMenuLoop
        4 -> actionGeneratePlantUML diagram >> mainMenuLoop diagram
        0 -> putStrLn "Returning to Initial Menu..."
        _ -> putStrLn "Invalid option." >> mainMenuLoop diagram

initialMenuLoop :: IO ()
initialMenuLoop = do
    putStrLn ""
    putStrLn "=== Initial Menu ==="
    putStrLn "1. Create a new diagram"
    putStrLn "0. Exit Program"
    choice <- getChoice
    case choice of
        1 -> do
            putStrLn "New diagram process started."
            actionSetSystemName initialDiagram >>= mainMenuLoop
        0 -> putStrLn "Goodbye!"
        _ -> putStrLn "Invalid option." >> initialMenuLoop

-- ==================================================
-- Program Entry Point
-- ==================================================
main :: IO ()
main = do
    putStrLn "Welcome to the Haskell Use Case Diagram Builder!"
    initialMenuLoop