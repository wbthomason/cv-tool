{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module CVTool.Types where 

import Control.Applicative

import Data.Aeson
import Data.Scientific (FPFormat(Fixed), formatScientific)
import Data.Time
import Data.Text (unpack)
import qualified Data.Vector as V

import GHC.Generics

import System.FilePath

data EitherStringNum = EitherStringNum String
  deriving (Generic)
instance FromJSON EitherStringNum where
  parseJSON s = EitherStringNum
    <$> 
      ((withText "String" (\x -> return $ unpack x) s) 
      <|> 
      (withScientific "Integer" (\x -> return $ formatScientific Fixed (Just 0) x) s))
instance ToJSON EitherStringNum

data CVLocation = CVLocation {
  address :: Maybe [String],
  postalCode :: Maybe EitherStringNum,
  city :: String,
  state :: String,
  countryCode :: String,
  region :: Maybe String
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
  label :: Maybe String,
  pictureLoc :: String,
  email :: String,
  phone :: String,
  personalSite :: Maybe String,
  objective :: Maybe String,
  location :: CVLocation,
  profiles :: [CVProfile],
  institution :: String,
  department :: String
}
  deriving (Generic)
instance FromJSON CVBasics
instance ToJSON CVBasics

data CVWork = CVWork {
  company :: String,
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
  gradeReceived :: Maybe String,
  comments :: Maybe String
}
  deriving (Generic)
instance FromJSON CVCourse
instance ToJSON CVCourse

data CVEducation = CVEducation {
  school :: String,
  majors :: [String],
  degree :: String,
  schoolSite :: String,
  schoolLocation :: CVLocation,
  studyStartDate :: String,
  studyEndDate :: Maybe String,
  gpa :: Maybe Float,
  courses :: Maybe [CVCourse]
}
  deriving (Generic)
instance FromJSON CVEducation
instance ToJSON CVEducation

data CVAwards = CVAwards {
  awardName :: String,
  dateAwarded :: String,
  awarder :: String,
  awardSummary :: Maybe String
}
  deriving (Generic)
instance FromJSON CVAwards
instance ToJSON CVAwards

data CVPublication = CVPublication {
  publicationType :: String,
  publicationYear :: EitherStringNum,
  authors :: [String],
  publicationVenue :: String,
  publicationTitle :: String,
  publicationLink :: Maybe String,
  publicationDescription :: Maybe String
}
  deriving (Generic)
instance FromJSON CVPublication
instance ToJSON CVPublication

data CVPublications = CVPublicationList [CVPublication] | CVPublicationFile FilePath 
  deriving (Generic)
instance FromJSON CVPublications where
  parseJSON v = (CVPublicationList <$> (withArray "List" (\x -> mapM parseJSON (V.toList x)) v)) <|> (CVPublicationFile <$> (withText "String" (\x -> return $ unpack x) v))
instance ToJSON CVPublications

data CVPresentation = CVPresentation {
  presentationTitle :: String,
  presentationLink :: Maybe String,
  presentationDescription :: Maybe String,
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
  skillKeywords :: Maybe [String]
}
  deriving (Generic)
instance FromJSON CVSkill
instance ToJSON CVSkill

data CVResearch = CVResearch {
  researchTopic :: String,
  researchDescription :: Maybe String
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
  interestKeywords :: Maybe [String]
}
  deriving (Generic)
instance FromJSON CVInterest
instance ToJSON CVInterest

data CVMembership = CVMembership {
  role :: String,
  organization :: String
}
  deriving (Generic)
instance FromJSON CVMembership
instance ToJSON CVMembership

data CVData = CVData { 
  basics :: CVBasics,
  work :: Maybe [CVWork],
  volunteering :: Maybe [CVWork],
  education :: [CVEducation],
  awards :: Maybe [CVAwards],
  publications :: Maybe CVPublications,
  presentations :: Maybe CVPresentations,
  skills :: Maybe [CVSkill],
  research :: Maybe [CVResearch],
  languages :: Maybe [CVLanguage],
  interests :: Maybe [CVInterest],
  memberships :: Maybe [CVMembership]
} 
  deriving (Generic)

instance FromJSON CVData
instance ToJSON CVData
