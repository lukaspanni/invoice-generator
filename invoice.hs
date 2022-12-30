module Invoice where

import Data.List
import Data.Time

data Customer = Customer { name :: String, company :: String, street :: String, zipCode :: String, city :: String, phone :: String }

instance Show Customer where 
  show (Customer name company street zipCode city phone) = name ++ " | " ++ company ++ " | " ++ street ++ ", " ++ zipCode ++ " " ++ city ++ " | " ++ phone

toYaml :: Customer -> IO String
toYaml (Customer name company street zipCode city phone) = do
  body <- ioBody
  return $ "---\n" ++ body ++ "\n..."
  where 
    ioBody = do
      date <- show <$> utctDay <$> getCurrentTime
      return $ "to: |\n" ++ (unlines $ map (\x -> "  " ++ x) addressLines) ++ "invoice: " ++ filter (/='-') date ++ "\n"
    addressLines
      | company == "" = [name, street, zipCode ++ " " ++ city]
      | otherwise = [company, name, street, zipCode ++ " " ++ city]



prettyPrint :: Customer -> [Int] -> String
prettyPrint c [] = prettyPrint c $ replicate 6 0 
prettyPrint (Customer name company street zipCode city phone) colWidth = (printCol name (colWidth!!0)) ++ " | " ++ (printCol company (colWidth!!1)) ++ " | " ++ (printCol street (colWidth!!2)) ++ ", " ++ (printCol zipCode (colWidth!!3)) ++ " " ++ (printCol city (colWidth!!4)) ++ " | " ++ (printCol phone (colWidth!!5))
  where 
    printCol :: String -> Int -> String
    printCol str colWidth
      | colWidth > (length str) = str ++ (replicate (colWidth - length str) ' ')
      | otherwise = str


splitFields :: String -> [String]
splitFields [] = []
splitFields (x:xs)
  | x == ',' = [] : splitFields xs
  | xs == [] = [[x]]
  | otherwise = (x:head (splitFields xs)) : tail (splitFields xs)


trim :: String -> String
trim = trimEnd . trimStart
  where 
    trimStart = dropWhile (==' ')
    trimEnd = reverse . trimStart . reverse


parseCustomers :: String -> [Customer]
parseCustomers = map parseCustomer . map (map trim) . map splitFields . drop 1 . lines
  where  
    parseCustomer entry = Customer { name = entry!!0, company = entry!!1, street = entry!!2, zipCode = entry!!3, city = entry!!4, phone = entry!!5 }


printCustomers :: [Customer] -> IO ()   
printCustomers customers = mapM_ (\(i,x) -> putStrLn (show  i ++ ") " ++ prettyPrint x (colWidths))) (zip [1..] customers) 
  where
    colWidths = map maximum $ transpose $ map (map length) $ map (\x -> [name x, company x, street x, zipCode x, city x, phone x]) customers 



createCustomerMetaDataFile :: Customer -> IO ()
createCustomerMetaDataFile customer = do 
  content <- toYaml customer
  writeFile ("customer-metadata.yaml") content



main :: IO ()
main = do
  customerData <- readFile "customer-data.csv"
  let customers = parseCustomers customerData
  putStrLn ("Select a customer to print an invoice for (Name | Company | Address | Phone):")
  printCustomers customers
  putStr ("Enter customer number (1-" ++ show (length customers) ++"): ")
  let inputNumber = readLn :: IO Int
  customerNumber <- inputNumber
  let customer = (customers!!(customerNumber-1)) 
  putStrLn $ "Selected Customer " ++ show customerNumber ++ ")  " ++ prettyPrint (customers!!(customerNumber-1)) [] 
  createCustomerMetaDataFile customer
  -- TODO: ask for invoice data (either as tuples or as csv path)
  -- TODO: create invoice data file (markdown-table)
  -- TODO: create invoice (pandoc)
