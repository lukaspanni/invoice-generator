module Invoice where

import Data.List
import Data.Time


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


data Customer = Customer { name :: String, company :: String, street :: String, zipCode :: String, city :: String, phone :: String }

instance Show Customer where 
  show (Customer name company street zipCode city phone) = name ++ " | " ++ company ++ " | " ++ street ++ ", " ++ zipCode ++ " " ++ city ++ " | " ++ phone


addressLines (Customer name company street zipCode city phone) = [company, name, street, zipCode ++ " " ++ city]

singleLineAddress customer = unwords $ addressLines customer

toYaml :: Customer -> String
toYaml customer = "---\n" ++ body ++ "\n..."
  where 
    body = "to: |\n" ++ (unlines $ map (\x -> "  " ++ x) $ filter (/="") $ addressLines customer)



prettyPrint :: Customer -> [Int] -> String
prettyPrint c [] = prettyPrint c $ replicate 6 0 
prettyPrint (Customer name company street zipCode city phone) colWidth = (printCol name (colWidth!!0)) ++ " | " ++ (printCol company (colWidth!!1)) ++ " | " ++ (printCol street (colWidth!!2)) ++ ", " ++ (printCol zipCode (colWidth!!3)) ++ " " ++ (printCol city (colWidth!!4)) ++ " | " ++ (printCol phone (colWidth!!5))
  where 
    printCol :: String -> Int -> String
    printCol str colWidth
      | colWidth > (length str) = str ++ (replicate (colWidth - length str) ' ')
      | otherwise = str

printCustomers :: [Customer] -> IO ()   
printCustomers customers = mapM_ (\(i,x) -> putStrLn (show  i ++ ") " ++ prettyPrint x (colWidths))) (zip [1..] customers) 
  where
    colWidths = map maximum $ transpose $ map (map length) $ map (\x -> [name x, company x, street x, zipCode x, city x, phone x]) customers 


parseCustomers :: String -> [Customer]
parseCustomers = map parseCustomer . map (map trim) . map splitFields . drop 1 . lines
  where  
    parseCustomer entry = Customer { name = entry!!0, company = entry!!1, street = entry!!2, zipCode = entry!!3, city = entry!!4, phone = entry!!5 }



data InvoiceListEntry = InvoiceListEntry {invoiceDate :: String, customer :: String, invoiceNumber :: Int} deriving Show


parseInvoiceList :: String -> [InvoiceListEntry]
parseInvoiceList = map parseInvoiceListEntry . map (map trim) . map splitFields . drop 1 . lines
  where 
    parseInvoiceListEntry entry = InvoiceListEntry {invoiceDate = entry!!0, customer = entry!!1, invoiceNumber = read (entry!!2) :: Int }


-- TODO: introduce typeclass if reused
toCSV :: [InvoiceListEntry] -> String
toCSV invoiceList = header ++ "\n" ++ (unlines $ map (\x -> (invoiceDate x) ++ "," ++ (customer x) ++ "," ++ (show $ invoiceNumber x)) invoiceList)
  where
    header = "Date, Customer, Invoice-Number"


date :: IO Day
date = utctDay <$> getCurrentTime

currentInvoiceDate :: IO String
currentInvoiceDate = filter (/='-') <$> show <$> date 


findStringIndex :: (Eq a) => [a] -> [a] -> Maybe Int
findStringIndex search str = findIndex (isPrefixOf search) (tails str)


createCustomerMetaDataFile :: Customer -> Int -> IO ()
createCustomerMetaDataFile customer invoiceNumber = do 
  currentInvoiceDate <- currentInvoiceDate
  let completeInvoiceNumber = currentInvoiceDate ++ show invoiceNumber
  let content = insertIntoYaml (toYaml customer) ("invoice: " ++ completeInvoiceNumber ++ "\n")
  writeFile ("customer-metadata.yaml") content
    where
      insertIntoYaml yaml str = case (findStringIndex "..." yaml) of
        Nothing -> yaml ++ str ++ "..."
        Just idx -> (take idx yaml) ++ str ++ (drop idx yaml)


updateInvoiceList :: [InvoiceListEntry] -> IO ()
updateInvoiceList invoiceList = do
  let content = toCSV invoiceList
  writeFile ("invoice-data.csv") content


main :: IO ()
main = do
  customerData <- readFile "customer-data.csv"
  let customers = parseCustomers customerData
  
  invoiceListData <- readFile "invoice-data.csv"
  let invoiceList = parseInvoiceList invoiceListData
  currentInvoiceDate <- currentInvoiceDate
  let currentInvoiceNumber = (foldl (max) 0 $ map (\x -> invoiceNumber x) $ filter (\x -> invoiceDate x == currentInvoiceDate) invoiceList) + 1

  putStrLn ("Select a customer to print an invoice for (Name | Company | Address | Phone):")
  printCustomers customers
  putStr ("Enter customer number (1-" ++ show (length customers) ++"): ")
  let inputNumber = readLn :: IO Int
  customerNumber <- inputNumber
  let customer = (customers!!(customerNumber-1)) 
  putStrLn $ "Selected Customer " ++ show customerNumber ++ ")  " ++ prettyPrint (customers!!(customerNumber-1)) [] 
  createCustomerMetaDataFile customer currentInvoiceNumber
  updateInvoiceList (invoiceList ++ [InvoiceListEntry {invoiceDate = currentInvoiceDate, customer = (singleLineAddress customer), invoiceNumber = currentInvoiceNumber}])
  -- TODO: ask for invoice data (either as tuples or as csv path)
  -- TODO: create invoice data file (markdown-table)
  -- TODO: create invoice (pandoc)
