module Main where
import Protolude hiding (local, intercalate, rotate, (<>))
import Prelude ((!!), tail)
import Data.Text (words, unpack, intercalate, pack)
import System.IO.Unsafe (unsafePerformIO)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Image
import Diagrams.Layout.Wrap
import Diagrams.TwoD.Layout.Constrained
import Diagrams.TwoD.Path.LSystem
import Diagrams.TwoD.Path.Turtle
import Text.Wrap
import Graphics.SVGFonts hiding (underline)

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = undefined

-- myLin = unsafePerformIO $ loadFont "CMUSerif-Roman.svg"

text' :: Double -> Text -> Diagram B
text' d s = strokeP (textSVG' (TextOpts lin INSIDE_H KERN False d d) (unpack s)) # lw none # fc black

underline :: Double -> Text -> Diagram B
underline d s = layout $ do
  str <- newDia $ text' d s
  line <- newScalableDia $ lw 1.0 $ fromOffsets [unitX]
  alignRight str line
  alignLeft str line
  constrainSepV 0.2 str line
  return ()

underline' :: Double -> Text -> Diagram B
underline' d s = layout $ do
  str <- newDia $ text' d s
  line <- newScalableDia $ lw 1.0 $ fromOffsets [unitX]
  alignRight str line
  alignLeft str line
  constrainSepV 0.0 str line
  return ()

defWrapSettings = WrapSettings { preserveIndentation = False, breakLongWords = True }

paragraph :: Int -> Text -> Diagram B
-- paragraph width = wrapDiagram . wrapInside (\p -> p^._x < width && p^._x >= 0 && p^._y <= 0) [unitX, unitY] (p2 (0, 0)) . map (text' 1 . unpack) . words
paragraph width = atPoints [p2 (0, y) | y <- [0, -1.1..]] . map (alignTL . text' 1) . wrapTextToLines defWrapSettings width

loadImage :: FilePath -> Diagram B
loadImage = image . fromRight . unsafePerformIO . loadImageEmb

class ResumePart a where
  renderPart :: a -> Constrained s B Double Any (DiaID s)

alignTop :: DiaID s -> DiaID s -> Constrained s B Double Any ()
alignTop a b = do
  aP <- newPointOn a (envelopeP unitY)
  bP <- newPointOn b (envelopeP unitY)
  aP ^. _y ==== bP ^. _y

alignBottom :: DiaID s -> DiaID s -> Constrained s B Double Any ()
alignBottom a b = do
  aP <- newPointOn a (envelopeP unit_Y)
  bP <- newPointOn b (envelopeP unit_Y)
  aP ^. _y ==== bP ^. _y

alignRight :: DiaID s -> DiaID s -> Constrained s B Double Any ()
alignRight a b = do
  aP <- newPointOn a (envelopeP unitX)
  bP <- newPointOn b (envelopeP unitX)
  aP ^. _x ==== bP ^. _x

alignLeft :: DiaID s -> DiaID s -> Constrained s B Double Any ()
alignLeft a b = do
  aP <- newPointOn a (envelopeP unit_X)
  bP <- newPointOn b (envelopeP unit_X)
  aP ^. _x ==== bP ^. _x

constrainSepV :: Expr s Double -> DiaID s -> DiaID s -> Constrained s B Double Any ()
constrainSepV sep a b = do
  aP <- newPointOn a (envelopeP unit_Y)
  bP <- newPointOn b (envelopeP unitY)
  bP ^. _y + sep ==== aP ^. _y

constrainSepH :: Expr s Double -> DiaID s -> DiaID s -> Constrained s B Double Any ()
constrainSepH sep a b = do
  aP <- newPointOn a (envelopeP unitX)
  bP <- newPointOn b (envelopeP unit_X)
  aP ^. _x + sep ==== bP ^. _x

data Header = Header
  { headerName :: Text
  , headerEmail :: Text
  , headerPhoneNumber :: Text
  }

instance ResumePart Header where
  renderPart h = do
    frame <- newScalableDia (fromOffsets [48 *^ unitX])
    name <- newDia $ text' 3 $ headerName h
    email <- newDia $ text' 1 $ headerEmail h
    number <- newDia $ text' 1 $ headerPhoneNumber h
    constrainWith (hsep 0.4) [email, number]
    alignLeft frame name
    alignRight frame number
    constrainSepV 0.2 name frame
    constrainSepV 0.2 number frame
    return frame


-- Experience

data Position = Position
  { positionTitle :: Text
  , positionPlace :: Text
  , positionDate :: Text
  , positionDesc :: Text
  , positionLogo :: Maybe FilePath
  }


instance ResumePart Position where
  renderPart pos = do
    place <- newDia $ alignTL $ text' 1.2 $ positionPlace pos
    title <- newDia $ alignTL $ text' 1 $ positionTitle pos
    date <- newDia $ alignTL $ text' 1 $ (<>")") $ ("("<>) $ positionDate pos
    desc <- newDia $ paragraph 80 $ positionDesc pos
    logo <- newScalableDia $ alignTL $ maybe (square 1 # lw none) loadImage $ positionLogo pos
    constrainWith (vsep 0.4) [place, title, desc]
    alignTop place logo
    alignBottom desc logo
    alignLeft title logo
    alignRight date desc
    alignTop title date
    return logo



-- Education

data School = School
  { schoolName :: Text
  , schoolDegree :: Text
  , schoolCourses :: [Course]
  , schoolGPA :: Maybe Double
  , schoolLogo :: Maybe FilePath
  }

data Course = Current Text | Completed Text

coursesToLine :: [Course] -> Text
coursesToLine = intercalate ", " . map disp
  where
    disp (Current course) = course <> "*"
    disp (Completed course) = course

type Education = [School]

instance ResumePart Education where
  renderPart = undefined

instance ResumePart School where
  renderPart sc = do
    name <- newDia $ alignTL $ text' 1.5 $ schoolName sc
    degree <- newDia $ alignTL $ text' 1 $ schoolDegree sc
    courses <- newDia $ alignBL $ paragraph 80 $ ("Courses: "<>) $ coursesToLine $ schoolCourses sc
    logo <- newScalableDia $ alignTL $ maybe (square 1 # lw none) (alignTL . loadImage) $ schoolLogo sc
    gpa <- newDia $ alignTL $ maybe mempty (text' 1 . ("GPA: "<>) . pack . show) $ schoolGPA sc
    constrainWith (vsep 0.4) [name, degree, courses]
    centerOf gpa ^. _y ==== centerOf degree ^. _y
    gpaRight <- newPointOn gpa (envelopeP unitX)
    coursesRight <- newPointOn courses (envelopeP unitX)
    gpaRight ^. _x ==== coursesRight ^. _x
    logoBottom <- newPointOn logo (envelopeP unit_Y)
    centerOf name =.= centerOf logo
    centerOf courses =.= logoBottom
    return logo


-- Projects

data Project = Project
  { projectTitle :: Text
  , projectDesc :: Text
  , projectScreenshot :: Maybe FilePath
  }

instance ResumePart Project where
  renderPart proj = do
    title <- newDia $ alignTL $ text' 1.5 $ projectTitle proj
    desc <- newDia $ alignTL $ paragraph 50 $ projectDesc proj
    constrainWith (vsep 0.2) [title, desc]
    scrshot <- newScalableDia $ maybe (square 1 # lw none) (alignTL . loadImage) $ projectScreenshot proj
    alignTop title scrshot
    alignRight desc scrshot
    alignLeft desc scrshot
    return scrshot

data Language = Language Text FilePath

instance ResumePart [Language] where
  renderPart [] = newDia $ fromOffsets [3 *^ unitY] # lw none
  renderPart ((Language lname fp):xs) = do
    prev <- renderPart xs
    name <- newDia $ alignTL $ text' 1 lname
    logo <- newScalableDia $ alignTL $ loadImage fp
    alignTop prev logo
    alignBottom prev logo
    constrainWith (vsep 0.5) [logo, name]
    constrainSepH 1 logo prev
    return logo

-- My stuff

myHeader = Header "Owen Lynch" "owen@olynch.me" "(617) 279-3222"

myExperience =
  [ Position {
      positionTitle = "Student Researcher"
    , positionPlace = "New England Complex Systems Institute"
    , positionDate = "2016-2017"
    , positionDesc = "Researched and implemented novel techniques in image processing. Wrote large-scale data scraping infrastructure. Helped to migrate large amounts of data. Studied novel information theoretic methods for modeling changes in complex systems. Worked in Python, R, and C."
    -- , positionLogo = Just "necsi_transparent.png" }
    , positionLogo = Nothing }
  , Position {
      positionTitle = "Math Resource Center Tutor"
    , positionPlace = "Brown Math Department"
    , positionDate = "2017"
    , positionDesc = "Tutored Brown students in calculus (up through multivariable) and linear algebra."
    , positionLogo = Nothing }
  , Position {
      positionTitle = "Math Classroom Assistant"
    , positionPlace = "Winter Hill Community School"
    , positionDate = "2014"
    , positionDesc = "Helped run a summer math class for struggling sixth to eighth grade students."
    , positionLogo = Nothing }
  , Position {
      positionTitle = "Web Developer Intern"
    , positionPlace = "Green Streets Initiative"
    , positionDate = "2013"
    , positionDesc = "Wrote live-updating leaderboard for Walk-Ride day competition. Implemented Django and Postgresql backend and JQuery frontend, and administered Debian server."
    -- , positionLogo = Just "greenstreets_transparent.png" }
    , positionLogo = Nothing }
  ]

myProjects =
  [ Project {
      projectTitle = "HackMIT 2017 \"Pathways\""
    , projectDesc = "Simulation of traffic routing algorithm to evacuate natural disasters. Won \"Hack with Most Startup Potential,\" \"Best Use of Location Data,\" and \"Moonshot.\" Written in Python using Networkx and Pygame."
    , projectScreenshot = Just "pathways_transparent.png" }
  , Project {
      projectTitle = "Interactive Graph Editor"
    , projectDesc = "Keyboard driven extensible graph editor, purpose is to explore nonlinear syntax for programming language design. Written in Haskell using Cairo."
    , projectScreenshot = Just "ige_transparent.png" }
  , Project {
      projectTitle = "Not Gonna Happen"
    , projectDesc = "Metronome that tracks your progress in a graphical calendar: if you don't practice every day it's not gonna happen! Written in Haskell using Yesod and Diagrams."
    , projectScreenshot = Just "ngh_transparent.png"
    }
  ]

myEducation = 
  [ School {
      schoolName = "Brown University"
    , schoolDegree = "Pursuing BSc., Math, May 2020"
    , schoolCourses = [
        Completed "Multivariable Calculus"
      , Completed "Linear Algebra"
      , Completed "Abstract Algebra"
      , Completed "Statistical Inference I"
      , Completed "Functions of Several Variables"
      , Completed "Algorithms"
      ]
    , schoolGPA = Just 4.0
    -- , schoolLogo = Just "brown_transparent.png" }
    , schoolLogo = Nothing }
  , School {
      schoolName = "Commonwealth School"
    , schoolDegree = "High School Diploma, June 2016"
    , schoolCourses = [
        Completed "Functions of a Single Variable"
      , Completed "Mathematical Logic"
      , Completed "Introduction to AI"
      , Completed "Introduction to Compilers"
      ]
    , schoolGPA = Nothing
    -- , schoolLogo = Just "cwealth_mermaid_transparent.png" }
    , schoolLogo = Nothing }
  ]

myLanguages =
  [ Language "Haskell" "haskell.png", Language "C" "c.png", Language "Python" "python.png", Language "Rust" "rust.png"
  , Language "NixOS" "nix-snowflake.png", Language "Ubuntu" "ubuntu.png"
  , Language "PostgreSQL" "postgresql.png"]


layoutPart part = layout $ void $ renderPart part

main :: IO ()
main = mainWith (bg white $ frame 2 $ vsep 1 [
    layoutPart myHeader
  , alignTL (underline 1.7 "Education")
  , hsep 1 (layoutPart <$> myEducation)
  , alignTL (underline' 1.7 "Projects")
  , hsep 1 (layoutPart <$> myProjects)
  , alignTL (underline' 1.7 "Experience")
  , hsep 1 (layoutPart <$> take 2 myExperience)
  , hsep 1 (layoutPart <$> tail (tail myExperience))
  , alignTL (underline 1.7 "Proficiencies (non-exhaustive)")
  , alignTL (layoutPart myLanguages)
  , hsep 1 [
      (vsep 1 [
        alignTL (underline 1.7 "Misc")
      , alignTL (paragraph 75 "I play violin, swing and blues dance, and rockclimb.")
      , alignTL (paragraph 75 "All logos are property of their respective projects and corporations, and do not imply any endorsement on the part of those projects and corporations.")
      ])
    , (vsep 1 [
        alignTL (underline 1.7 "Resume written in Haskell so: Fractals!")
      , hsep 1 [
          stroke (getTurtlePath (dragon 9)) # lw 0.7 # scale 0.2 # alignTL
        , stroke (getTurtlePath (tree6 5)) # lw 0.7 # scale 0.07 # rotate ((1/4) @@ turn) # alignTL
        , stroke (getTurtlePath (kochLake 2)) # lw 0.7 # scale 0.1 # alignTL
        , stroke (getTurtlePath (koch4 4)) # lw 0.7 # scale 0.07 # alignTL
        ]
      ])
    ]
  ])
