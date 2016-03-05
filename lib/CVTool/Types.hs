{-# LANGUAGE DeriveGeneric #-}
module CVTool.Types where 

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Time

import GHC.Generics

import System.FilePath

data CVLocation = CVLocation {
  address :: String,
  postalCode :: String,
  city :: String,
  countryCode :: String,
  region :: String
}
  deriving (Generic)
instance FromJSON CVLocation
instance ToJSON CVLocation

data CVProfile = CVProfile {
  network :: String,
  username :: String,
  url :: String
}
  deriving (Generic)
instance FromJSON CVProfile
instance ToJSON CVProfile

data CVBasics = CVBasics {
  name :: String,
  label :: String,
  pictureLoc :: String,
  email :: String,
  phone :: String,
  personalSite :: String,
  personalSummary :: String,
  location :: CVLocation,
  profiles :: [CVProfile]
}
  deriving (Generic)
instance FromJSON CVBasics
instance ToJSON CVBasics

data CVWork = CVWork {
  organization :: String,
  position :: String,
  companyWebsite :: String,
  startDate :: String,
  endDate :: String,
  workSummary :: String,
  highlights :: [String]
}
  deriving (Generic)
instance FromJSON CVWork
instance ToJSON CVWork

data CVCourse = CVCourse {
  courseName :: String,
  courseMnemonic :: String,
  yearTaken :: String,
  gradeReceived :: String,
  comments :: String
}
  deriving (Generic)
instance FromJSON CVCourse
instance ToJSON CVCourse

data CVEducation = CVEducation {
  institution :: String,
  majors :: [String],
  degreeEarned :: String,
  studyStartDate :: String,
  studyEndDate :: String,
  gpa :: Float,
  courses :: [CVCourse]
}
  deriving (Generic)
instance FromJSON CVEducation
instance ToJSON CVEducation

data CVAwards = CVAwards {
  awardName :: String,
  dateAwarded :: String,
  awarder :: String,
  awardSummary :: String
}
  deriving (Generic)
instance FromJSON CVAwards
instance ToJSON CVAwards

data CVPublication = CVPublication {
  publicationType :: String,
  publicationDate :: String,
  authors :: [String],
  publicationVenue :: String,
  publicationTitle :: String,
  publicationLink :: String
}
  deriving (Generic)
instance FromJSON CVPublication
instance ToJSON CVPublication

data CVPublications = CVPublicationFile FilePath | CVPublicationList [CVPublication]
  deriving (Generic)
instance FromJSON CVPublications
instance ToJSON CVPublications

data CVPresentation = CVPresentation {
  presentationTitle :: String,
  presentationLink :: String,
  presentationVenue :: String,
  presentationDate :: String
}
  deriving (Generic)
instance FromJSON CVPresentation
instance ToJSON CVPresentation

data CVPresentations = CVPresentationFile FilePath | CVPresentationList [CVPresentation]
  deriving (Generic)
instance FromJSON CVPresentations
instance ToJSON CVPresentations

data CVSkill = CVSkill {
  skillName :: String,
  skillLevel :: String,
  skillKeywords :: [String]
}
  deriving (Generic)
instance FromJSON CVSkill
instance ToJSON CVSkill

data CVResearch = CVResearch {
  researchTopic :: String,
  researchDescription :: String
}
  deriving (Generic)
instance FromJSON CVResearch
instance ToJSON CVResearch

data CVLanguage = CVLanguage {
  language :: String,
  fluency :: String
}
  deriving (Generic)
instance FromJSON CVLanguage
instance ToJSON CVLanguage

data CVInterest = CVInterest {
  interestName :: String,
  interestKeywords :: [String]
}
  deriving (Generic)
instance FromJSON CVInterest
instance ToJSON CVInterest

data CVData = CVData { 
  basics :: CVBasics,
  work :: [CVWork],
  volunteering :: [CVWork],
  education :: [CVEducation],
  awards :: [CVAwards],
  publications :: CVPublications,
  presentations :: CVPresentations,
  skills :: [CVSkill],
  research :: CVResearch,
  languages :: [CVLanguage],
  interests :: [CVInterest]
  } 
  deriving (Generic)

instance FromJSON CVData
instance ToJSON CVData
