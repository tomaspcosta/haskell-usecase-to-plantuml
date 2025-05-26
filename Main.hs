-- main.hs
-- Use Case Diagram Generator in Haskell
-- This program defines data structures for use case diagrams and functions
-- to add elements, validate relationships, and generate PlantUML code.

import Data.List (isInfixOf, nub, intercalate)
import System.IO (writeFile, IOMode(WriteMode), hClose, openFile)
import Data.Char (isAlphaNum, toLower)

-- ==================================================
-- Data Type Definitions
-- ==================================================

-- Type alias for names of actors, use cases, etc.
type ElementName = String

-- Data type for Actors
data Actor = Actor ElementName deriving (Show, Eq, Ord)

-- Data type for Use Cases
data UseCase = UseCase ElementName deriving (Show, Eq, Ord)

-- Type alias for the System (Package) Name
type SystemName = ElementName

-- Data type for Relationships
-- Note: Requirement [cite: 4] mentions "relations between use cases".
-- For a complete diagram, other relations are also included here.
data Relationship
    = Association ElementName ElementName -- ActorName, UseCaseName
    | Include ElementName ElementName     -- BaseUCName, IncludedUCName
    | Extend ElementName ElementName      -- ExtendingUCName, ExtendedUCName
    | GeneralizationUseCase ElementName ElementName -- ChildUCName, ParentUCName
    | GeneralizationActor ElementName ElementName   -- ChildActorName, ParentActorName
    deriving (Show, Eq)

-- Main data structure for the Use Case Diagram
data UseCaseDiagram = UseCaseDiagram {
    systemName    :: Maybe SystemName,
    actors        :: [Actor],
    useCases      :: [UseCase],
    relationships :: [Relationship]
} deriving (Show, Eq)

-- Initial empty diagram
initialDiagram :: UseCaseDiagram
initialDiagram = UseCaseDiagram Nothing [] [] []

-- ==================================================
-- Helper Functions for Element Checking
-- ==================================================

-- Checks if an actor with the given name exists in the diagram
actorExists :: ElementName -> UseCaseDiagram -> Bool
actorExists name diagram = Actor name `elem` actors diagram

-- Checks if a use case with the given name exists in the diagram
useCaseExists :: ElementName -> UseCaseDiagram -> Bool
useCaseExists name diagram = UseCase name `elem` useCases diagram

-- Checks if a relationship already exists in the diagram
relationshipExists :: Relationship -> UseCaseDiagram -> Bool
relationshipExists rel diagram = rel `elem` relationships diagram

-- ==================================================
-- Functions for "Inserting" Specifications [cite: 4]
-- ==================================================

-- Adds an actor to the diagram, ensuring no duplicates.
-- Returns Maybe UseCaseDiagram: Just diagram if successful, Nothing if actor already exists.
addActor :: ElementName -> UseCaseDiagram -> Maybe UseCaseDiagram
addActor name diagram
    | actorExists name diagram = Nothing -- Actor already exists
    | otherwise = Just diagram { actors = nub (Actor name : actors diagram) }

-- Adds a use case to the diagram, ensuring no duplicates.
-- Returns Maybe UseCaseDiagram: Just diagram if successful, Nothing if use case already exists.
addUseCase :: ElementName -> UseCaseDiagram -> Maybe UseCaseDiagram
addUseCase name diagram
    | useCaseExists name diagram = Nothing -- Use case already exists
    | otherwise = Just diagram { useCases = nub (UseCase name : useCases diagram) }

-- Sets or updates the system (package) name for the diagram.
setSystemName :: SystemName -> UseCaseDiagram -> UseCaseDiagram
setSystemName name diagram = diagram { systemName = Just name }

-- Validates and adds a relationship to the diagram. [cite: 5]
-- Requirement [cite: 5] specifically asks to validate if *use cases* involved are defined.
-- A more robust validation would check all participating elements (actors and use cases).
-- This implementation adheres to the strict wording for UC validation,
-- but also checks actors for associations and actor generalizations.
addRelationship :: Relationship -> UseCaseDiagram -> Maybe UseCaseDiagram
addRelationship rel diagram
    | relationshipExists rel diagram = Nothing -- Relationship already exists
    | not (validateRelationship rel diagram) = Nothing -- Validation failed
    | otherwise = Just diagram { relationships = nub (rel : relationships diagram) }

-- Validation logic for relationships [cite: 5]
validateRelationship :: Relationship -> UseCaseDiagram -> Bool
validateRelationship (Association actorName ucName) diagram =
    actorExists actorName diagram && useCaseExists ucName diagram
validateRelationship (Include baseUCName includedUCName) diagram =
    useCaseExists baseUCName diagram && useCaseExists includedUCName diagram && baseUCName /= includedUCName
validateRelationship (Extend extendingUCName extendedUCName) diagram =
    useCaseExists extendingUCName diagram && useCaseExists extendedUCName diagram && extendingUCName /= extendedUCName
validateRelationship (GeneralizationUseCase childUCName parentUCName) diagram =
    useCaseExists childUCName diagram && useCaseExists parentUCName diagram && childUCName /= parentUCName
validateRelationship (GeneralizationActor childActorName parentActorName) diagram =
    actorExists childActorName diagram && actorExists parentActorName diagram && childActorName /= parentActorName

-- ==================================================
-- PlantUML Code Generation [cite: 6]
-- ==================================================

-- Generates a PlantUML safe name (lowercase, no spaces/hyphens, prefixed if starts with digit)
sanitizeName :: ElementName -> String
sanitizeName name =
    let lowerName = map toLower name
        noSpaces = map (\c -> if c == ' ' || c == '-' then '_' else c) lowerName
        alphaNumOnly = filter isAlphaNum noSpaces -- Keep only alphanumeric after initial replacement
        validId = if null alphaNumOnly then "unspecified_id" else alphaNumOnly
    in if not (null validId) && head validId >= '0' && head validId <= '9'
           then "id_" ++ validId
           else validId

-- Converts an Actor to its PlantUML string representation
actorToPlantUML :: Actor -> String
actorToPlantUML (Actor name) =
    "actor \"" ++ name ++ "\" as " ++ sanitizeName name

-- Converts a UseCase to its PlantUML string representation
useCaseToPlantUML :: UseCase -> String
useCaseToPlantUML (UseCase name) =
    "usecase \"" ++ name ++ "\" as " ++ sanitizeName name

-- Converts a Relationship to its PlantUML string representation
relationshipToPlantUML :: Relationship -> String
relationshipToPlantUML rel = case rel of
    Association actorName ucName ->
        sanitizeName actorName ++ " -- " ++ sanitizeName ucName
    Include baseUC includedUC ->
        sanitizeName baseUC ++ " ..> " ++ sanitizeName includedUC ++ " : <<include>>"
    Extend extendingUC extendedUC ->
        sanitizeName extendingUC ++ " ..> " ++ sanitizeName extendedUC ++ " : <<extend>>"
    GeneralizationUseCase childUC parentUC ->
        sanitizeName childUC ++ " --|> " ++ sanitizeName parentUC
    GeneralizationActor childActor parentActor ->
        sanitizeName childActor ++ " --|> " ++ sanitizeName parentActor

-- Converts the entire UseCaseDiagram to a PlantUML string
diagramToPlantUML :: UseCaseDiagram -> String
diagramToPlantUML diagram =
    unlines $
    ["@startuml"] ++
    title ++
    ["left to right direction", "skinparam packageStyle rectangle", "skinparam actorStyle awesome", ""] ++
    map actorToPlantUML (actors diagram) ++
    [""] ++
    packageStart ++
    map useCaseToPlantUML (useCases diagram) ++
    packageEnd ++
    [""] ++
    map relationshipToPlantUML (relationships diagram) ++
    ["@enduml"]
  where
    title = case systemName diagram of
              Just sn -> ["title Use Case Diagram for " ++ sn, ""]
              Nothing -> ["title Use Case Diagram", ""]
    packageStart = case systemName diagram of
                     Just sn -> ["rectangle \"<<System>>\\n" ++ sn ++ "\" as " ++ sanitizeName sn ++ "_boundary {", ""]
                     Nothing -> []
    packageEnd = case systemName diagram of
                   Just _  -> ["", "}"]
                   Nothing -> []


-- ==================================================
-- Main function to demonstrate usage
-- ==================================================

-- Safely adds an element or relationship, printing errors if any
-- This is a helper for the demonstration in `main`
addComponent :: (ElementName -> UseCaseDiagram -> Maybe UseCaseDiagram)
             -> ElementName
             -> UseCaseDiagram
             -> IO UseCaseDiagram
addComponent addFunc name diagram =
    case addFunc name diagram of
        Just newDiagram -> return newDiagram
        Nothing         -> do
            putStrLn $ "Error: Could not add '" ++ name ++ "'. It might already exist or be invalid."
            return diagram

addRel :: Relationship
       -> UseCaseDiagram
       -> IO UseCaseDiagram
addRel rel diagram =
    case addRelationship rel diagram of
        Just newDiagram -> return newDiagram
        Nothing         -> do
            putStrLn $ "Error: Could not add relationship '" ++ show rel ++ "'. It might already exist or involve undefined/invalid elements."
            return diagram

-- Example usage
main :: IO ()
main = do
    putStrLn "--- Building Use Case Diagram ---"

    let diagram0 = initialDiagram
    let diagram1 = setSystemName "Library Management System" diagram0

    -- Add Actors
    diagram2 <- addComponent addActor "Librarian" diagram1
    diagram3 <- addComponent addActor "Member" diagram2

    -- Add Use Cases
    diagram4 <- addComponent addUseCase "Borrow Book" diagram3
    diagram5 <- addComponent addUseCase "Return Book" diagram4
    diagram6 <- addComponent addUseCase "Search Catalog" diagram5
    diagram7 <- addComponent addUseCase "Manage Users" diagram6

    -- Add Relationships
    diagram8 <- addRel (Association "Member" "Borrow Book") diagram7
    diagram9 <- addRel (Association "Member" "Return Book") diagram8
    diagram10 <- addRel (Association "Member" "Search Catalog") diagram9
    diagram11 <- addRel (Association "Librarian" "Manage Users") diagram10
    diagram12 <- addRel (Include "Borrow Book" "Search Catalog") diagram11 -- Member must search catalog to borrow
    diagram13 <- addRel (Extend "Borrow Book" "Request InterLibrary Loan") diagram12 -- Add an extend, UC "Request ILL" not added yet for demo of validation
    
    -- Let's add the missing UC for the extend to be valid (or it would fail)
    diagram13_fixed <- addComponent addUseCase "Request InterLibrary Loan" diagram12
    diagram14 <- addRel (Extend "Borrow Book" "Request InterLibrary Loan") diagram13_fixed
    diagram15 <- addRel (GeneralizationActor "Student Member" "Member") diagram14 -- Actor "Student Member" not added yet

    -- Add missing actor for generalization
    diagram15_fixed <- addComponent addActor "Student Member" diagram14
    diagram16 <- addRel (GeneralizationActor "Student Member" "Member") diagram15_fixed


    let finalDiagram = diagram16 -- Or diagram12 if you want to see validation failure for Extend/Generalization

    putStrLn "\n--- Final Diagram Specification (Haskell Data) ---"
    print finalDiagram

    let plantUMLCode = diagramToPlantUML finalDiagram
    putStrLn "\n--- Generated PlantUML Code ---"
    putStrLn plantUMLCode

    -- Write to file
    let outputFilename = "use_case_diagram.puml"
    handle <- openFile outputFilename WriteMode
    hPutStrLn handle plantUMLCode
    hClose handle
    putStrLn $ "\nPlantUML code saved to " ++ outputFilename
    putStrLn "--- Program End ---"