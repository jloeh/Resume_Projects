--SELECT TOP (1000) [UniqueID ]
--      ,[ParcelID]
--      ,[LandUse]
--      ,[PropertyAddress]
--      ,[SaleDate]
--      ,[SalePrice]
--      ,[LegalReference]
--      ,[SoldAsVacant]
--      ,[OwnerName]
--      ,[OwnerAddress]
--      ,[Acreage]
--      ,[TaxDistrict]
--      ,[LandValue]
--      ,[BuildingValue]
--      ,[TotalValue]
--      ,[YearBuilt]
--      ,[Bedrooms]
--      ,[FullBath]
--      ,[HalfBath]
--  FROM [master].[dbo].[NashvilleHousing]

--###Clean DATES#### 

--SELECT * 
--FROM NashvilleHousing

--SELECT SaleDateConvert, CONVERT(Date, SaleDate) as Date1
--FROM NashvilleHousing 

--Update NashvilleHousing
--SET SaleDate = CONVERT(Date, SaleDate) 

--ALTER TABLE NashvilleHousing
--Add SaleDateConvert Date;

--Update NashvilleHousing
--SET SaleDateConvert = CONVERT(Date, SaleDate) 

--###FILL IN NULL ADDRESSES####

--SELECT *
--FROM NashvilleHousing
--WHERE PropertyAddress is null

--SELECT *
--FROM NashvilleHousing
--WHERE PropertyAddress is null
--ORDER BY ParcelID

--SELECT a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
--FROM NashvilleHousing a
--JOIN NashvilleHousing b
--	on a.ParcelID = b.ParcelID
--	AND a.[UniqueID ] <> b.[UniqueID ]
--WHERE a.PropertyAddress is null

--UPDATE a
--SET PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
--FROM NashvilleHousing a
--JOIN NashvilleHousing b
--	on a.ParcelID = b.ParcelID
--	AND a.[UniqueID ] <> b.[UniqueID ]
--WHERE a.PropertyAddress is null

--##ISNULL CHECKS TO SEE IF IT IS NULL AND THEN YOU CAN POPULATE WITH A VALUE##

--SELECT PropertyAddress 
--FROM NashvilleHousing


--##SPLIT ADDRESS AND CITY###


--SELECT 
--SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1) as Address
--, SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress)+1, LEN(PropertyAddress)) as City
--FROM NashvilleHousing

--##This says look at property address start at the first character and search for the comma in property address then move one
--##space back from the comma and cut there, then create another row and go one space beyond the comma

--ALTER TABLE NashvilleHousing
--Add PropertySplitAddress Nvarchar(255);

--UPDATE NashvilleHousing
--SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1)

--ALTER TABLE NashvilleHousing
--Add PropertySplitCity Nvarchar(255);

--UPDATE NashvilleHousing
--SET PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress)+1, LEN(PropertyAddress)) 

--SELECT * 
--FROM NashvilleHousing


--##SPLIT OWNER ADDRESS

--SELECT OwnerAddress
--FROM NashvilleHousing

--SELECT
--PARSENAME(REPLACE(OwnerAddress, ',', '.'), 3),
--PARSENAME(REPLACE(OwnerAddress, ',', '.'), 2),
--PARSENAME(REPLACE(OwnerAddress, ',', '.'), 1) as State
--FROM NashvilleHousing

----##PARSENAME LOOKS FOR PERIODS SO REPLACE COMMAS WITH PERIODS TO USE PARSENAME AND IT BREAKS OUT EACH CHUNK 

--ALTER TABLE NashvilleHousing
--Add OwnerSplitAddres Nvarchar(255)

--Update NashvilleHousing
--Set OwnerSplitAddres = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 3)

--ALTER TABLE NashvilleHousing
--Add OwnerSplitCity Nvarchar(255)

--Update NashvilleHousing
--Set OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 2)

--ALTER TABLE NashvilleHousing
--Add OwnerSplitState Nvarchar(255)

--Update NashvilleHousing
--Set OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 1)


--SELECT * 
--FROM NashvilleHousing


--###CLEANING SOLD AS VACANT##

--SELECT Distinct(SoldAsVacant), COUNT(SoldAsVacant)
--FROM NashvilleHousing
--GROUP BY SoldAsVacant
--ORDER BY 2

--SELECT SoldAsVacant,
--	CASE WHEN SoldAsVacant = 'Y' THEN 'Yes'
--	WHEN SoldAsVacant = 'N' THEN 'No'
--	ELSE SoldAsVacant
--	END
--FROM NashvilleHousing

--UPDATE NashvilleHousing
--SET SoldAsVacant = CASE WHEN SoldAsVacant = 'Y' THEN 'Yes'
--	WHEN SoldAsVacant = 'N' THEN 'No'
--	ELSE SoldAsVacant
--	END

--SELECT DISTINCT(SoldAsVacant), COUNT(SoldAsVacant)
--FROM NashvilleHousing
--GROUP BY SoldAsVacant
--ORDER BY 2



--##REMOVE DUPLICATES####

--WITH RowNumCTE AS(
--SELECT *, 
--ROW_NUMBER() OVER (
--PARTITION BY ParcelID,
--			PropertyAddress,
--			SalePrice,
--			SaleDate, 
--			LegalReference
--			ORDER BY UniqueId) as row_num
--FROM NashvilleHousing)

--SELECT * 
--FROM RowNumCTE
--WHERE row_num >1
--ORDER BY PropertyAddress

----#TO DELETE THESE CHANGE SELECT ALL TO DELETE 

--WITH RowNumCTE AS(
--SELECT *, 
--ROW_NUMBER() OVER (
--PARTITION BY ParcelID,
--			PropertyAddress,
--			SalePrice,
--			SaleDate, 
--			LegalReference
--			ORDER BY UniqueId) as row_num
--FROM NashvilleHousing)

--DELETE
--FROM RowNumCTE
--WHERE row_num >1


--###DELETE UNUSED COLUMNS###

--SELECT * 
--FROM NashvilleHousing

--ALTER TABLE NashvilleHousing
--DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress

--ALTER TABLE NashvilleHousing
--DROP COLUMN SaleDate

--SELECT * 
--FROM NashvilleHousing