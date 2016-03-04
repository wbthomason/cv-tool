{-# LANGUAGE DeriveGeneric #-}
module CVTool.Types where 

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics
import Data.Time

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
  picture_loc :: String,
  email :: String,
  phone :: String,
  personal_site :: String,
  personal_summary :: String,
  location :: CVLocation,
  profiles :: [CVProfile]
}
  deriving (Generic)
instance FromJSON CVBasics
instance ToJSON CVBasics

data CVWork = CVWork {
  organization :: String,
  position :: String,
  company_website :: String,
  startDate :: String,
  endDate :: String,
  work_summary :: String,
  highlights :: [String]
}
  deriving (Generic)
instance FromJSON CVWork
instance ToJSON CVWork

data CVCourse = CVCourse {
  course_name :: String,
  course_mnemonic :: String,
  year_taken :: String,
  grade_received :: String,
  comments :: String
}
  deriving (Generic)
instance FromJSON CVCourse
instance ToJSON CVCourse

data CVEducation = CVEducation {
  institution :: String,
  majors :: [String],
  degree_earned :: String,
  study_startDate :: String,
  study_endDate :: String,
  gpa :: Float,
  courses :: [CVCourse]
}
  deriving (Generic)
instance FromJSON CVEducation
instance ToJSON CVEducation

data CVAwards = CVAwards {
  award_name :: String,
  date_awarded :: String,
  awarder :: String,
  award_summary :: String
}
  deriving (Generic)
instance FromJSON CVAwards
instance ToJSON CVAwards

data CVPublication = CVPublication {
  publication_type :: String,
  publication_date :: String,
  authors :: [String],
  venue :: String,
    publication_title :: String,
  publication_link :: String
}

data CVPublications = CitationFile | [CVPublication]
  deriving (Generic)
instance FromJSON CVPublications
instance ToJSON CVPublications

data CVPresentations = CVPresentations
  deriving (Generic)
instance FromJSON CVPresentations
instance ToJSON CVPresentations

data CVSkill = CVSkill
  deriving (Generic)
instance FromJSON CVSkill
instance ToJSON CVSkill

data CVResearch = CVResearch
  deriving (Generic)
instance FromJSON CVResearch
instance ToJSON CVResearch

data CVLanguage = CVLanguage
  deriving (Generic)
instance FromJSON CVLanguage
instance ToJSON CVLanguage

data CVInterest = CVInterest
  deriving (Generic)
instance FromJSON CVInterest
instance ToJSON CVInterest

data CVReference = CVReference
  deriving (Generic)
instance FromJSON CVReference
instance ToJSON CVReference

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
  interests :: [CVInterest],
  references :: [CVReference]
  } 
  deriving (Generic)

instance FromJSON CVData
instance ToJSON CVData
