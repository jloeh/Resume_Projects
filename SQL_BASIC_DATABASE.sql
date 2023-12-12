--CREATE TABLE EmployeeDemo
--(EmployeeID int,
--FirstName varchar(50),
--LastName varchar(50),
--Age int,
--Gender varchar (50))

--CREATE TABLE EmployeeSalary
--(EmployeeID int,
--JobTitle varchar (50),
--Salary int)

--INSERT INTO EmployeeDemo VALUES
--(1002, 'Pam', 'Beasley', 30, 'Female'),
--(1003, 'Dwight', 'Schrute', 29, 'Male'),
--(1004, 'Angela', 'Martin', 31, 'Female'),
--(1005, 'Toby', 'Flenderson', 32, 'Male'),
--(1006, 'Michael', 'Scott', 35, 'Male'),
--(1007, 'Meredith', 'Palmer', 32, 'Female'),
--(1008, 'Stanley', 'Hudson', 38, 'Male'),
--(1009, 'Kevin', 'Malone', 31, 'Male')

--INSERT INTO EmployeeSalary VALUES
--(1002, 'Receptionist', 36000),
--(1003, 'Salesman', 63000),
--(1004, 'Accountant', 47000),
--(1005, 'HR', 50000),
----(1006, 'Regional Manager', 65000),
----(1007, 'Supplier Relations', 41000),
----(1008, 'Salesman', 48000),
----(1009, 'Accountant', 42000)


--CREATE TABLE EmployeeSalary
--(EmployeeID int,
--JobTitle varchar (50),
--Salary int)

--INSERT INTO EmployeeSalary VALUES
--(1001, 'Salesman', 45000),
--(1002, 'Receptionist', 36000),
--(1003, 'Salesman', 63000),
--(1004, 'Accountant', 47000),
--(1005, 'HR', 50000),
--(1006, 'Regional Manager', 65000),
--(1007, 'Supplier Relations', 41000),
--(1008, 'Salesman', 48000),
--(1009, 'Accountant', 42000)

--####

--SELECT AVG(Salary)
--FROM EmployeeSalary

--SELECT MIN or MAX(Salary)
--FROM EmployeeSalary

--SELECT DISTINCT (JobTitle)
--FROM EmployeeSalary

--SELECT COUNT(EmployeeID)
--FROM EmployeeSalary

--SELECT * 
--FROM EmployeeSalary

--#IF YOU ARE USING DIFFERENT DATABASES 
--SELECT * 
--FROM [SQL Tutorial].dbo.EmployeeSalary


--####

--SELECT * 
--FROM EmployeeDemo
--WHERE FirstName = 'Jim'

--SELECT * 
--FROM EmployeeDemo
--WHERE FirstName <> 'Jim' #This means does not equal 

--SELECT * 
--FROM EmployeeDemo
--WHERE Age > 30 #Greater than 30 

--SELECT * 
--FROM EmployeeDemo
--WHERE Age >= 30 #Greater than or equal to 30 

--SELECT * 
--FROM EmployeeDemo
--WHERE Age <= 32 AND Gender = 'Male'

--SELECT * 
--FROM EmployeeDemo
--WHERE Age <= 32 OR Gender = 'Male'  ## If you OR only need one of these

--SELECT * 
--FROM EmployeeDemo
--WHERE LastName LIKE 'S%' ##This means every last name that starts with S and then has characters after the '%' is a wild card
--If have %S% this means that show if there is an S anywhere in the name

--SELECT * 
--FROM EmployeeDemo
--WHERE LastName LIKE 'S%o%'


--SELECT * 
--FROM EmployeeDemo
--WHERE FirstName is NOT NULL

--SELECT * 
--FROM EmployeeDemo
--WHERE FirstName is NULL

--SELECT * 
--FROM EmployeeDemo
--WHERE FirstName IN ('Jim', 'Michael')
--#Multiple equal statment instead of statement below 

--SELECT * 
--FROM EmployeeDemo
--WHERE FirstName = 'Jim' OR FirstName = 'Michael'

--#####

--GROUP AND ORDER BY 

--SELECT Gender, COUNT(Gender)
--FROM EmployeeDemo
--GROUP BY Gender

--SELECT Gender
--FROM EmployeeDemo
--GROUP BY Gender

--SELECT * 
--FROM EmployeeDemo

--SELECT Gender, Age, COUNT(Gender)
--FROM EmployeeDemo
--GROUP BY Gender, Age
--Counts people who are same gender and age 


--SELECT Gender, COUNT(Gender)
--FROM EmployeeDemo
--Where Age >31
--GROUP BY Gender

 
--SELECT Gender, COUNT(Gender) AS CountGender
--FROM EmployeeDemo
--Where Age >31
--GROUP BY Gender
--ORDER BY Gender 

--SELECT * 
--FROM EmployeeDemo
--ORDER BY Age ASC, Gender DESC

--SELECT * 
--FROM EmployeeDemo
--ORDER BY 4 ASC, 5 DESC
--## the numbers refer to the columns 

--###INTERMEDIATE SQL#####

--JOINS

--DELETE 
--FROM EmployeeDemo
--WHERE FirstName IN('Ryan', 'Holly', 'Darryl')

--Insert into EmployeeDemo VALUES
--(1011, 'Ryan', 'Howard', 26, 'Male'),
--(NULL, 'Holly', 'Flax', NULL, NULL),
--(1013, 'Darryl', 'Philbin', NULL, 'Male')


--INSERT into EmployeeSalary VALUES
--(1010, Null, 47000),
--(NULL, 'Salesman', 43000)

--SELECT * FROM EmployeeDemo

--SELECT * FROM EmployeeSalary

--INNER JOIN shows everything that is the same 
--SELECT * 
--FROM [SQL Tutorial].dbo.EmployeeDemo
--Inner Join [SQL Tutorial].dbo.EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID

--SELECT * 
--FROM EmployeeDemo
--FULL OUTER JOIN EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID
--#THIS SHOWS EVERYTHING FROM BOTH DATABASES 

--SELECT * 
--FROM [SQL Tutorial].dbo.EmployeeDemo
--LEFT OUTER Join [SQL Tutorial].dbo.EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID
--#LEFT TABLE IS FIRST TABLE AND IT SHOWS EVERYTHING IN LEFT AND WHATEVER MATCHES IN RIGHT 

--SELECT * 
--FROM [SQL Tutorial].dbo.EmployeeDemo
--RIGHT OUTER Join [SQL Tutorial].dbo.EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID

--SELECT EmployeeID, FirstName, LastName, JobTitle, Salary
--FROM [SQL Tutorial].dbo.EmployeeDemo
--RIGHT OUTER JOIN [SQL Tutorial].dbo.EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID
--#THIS DOES NOT WORK BECAUSE NEED TO SPECIFY WHICH TABLES EMPLOYEE ID 

--SELECT EmployeeDemo.EmployeeID, FirstName, LastName, JobTitle, Salary
--FROM [SQL Tutorial].dbo.EmployeeDemo
--LEFT  OUTER JOIN [SQL Tutorial].dbo.EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID

--SELECT EmployeeDemo.EmployeeID, FirstName, LastName, Salary
--FROM [SQL Tutorial].dbo.EmployeeDemo
--INNER JOIN [SQL Tutorial].dbo.EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID
--WHERE FirstName <> 'Michael'
--ORDER BY Salary DESC


--SELECT JobTitle, AVG(Salary) AS AVGSALARY
--FROM [SQL Tutorial].dbo.EmployeeDemo
--INNER JOIN [SQL Tutorial].dbo.EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID
--WHERE JobTitle = 'Salesman'
--GROUP BY JobTitle


--Create Table Warehouse
--(EmployeeID int, 
--FirstName varchar(50), 
--LastName varchar(50), 
--Age int, 
--Gender varchar(50)
--)


--Insert into Warehouse VALUES
--(1013, 'Darryl', 'Philbin', NULL, 'Male'),
--(1050, 'Roy', 'Anderson', 31, 'Male'),
--(1051, 'Hidetoshi', 'Hasagawa', 40, 'Male'),
--(1052, 'Val', 'Johnson', 31, 'Female')

--#####UNIONS######### Joins are combined off of columns whereas unions take everything 

--SELECT*
--FROM EmployeeDemo
--FULL OUTER JOIN Warehouse
--	ON EmployeeDemo.EmployeeID = Warehouse.EmployeeID

--SELECT * 
--FROM EmployeeDemo
--UNION
--SELECT * 
--FROM Warehouse
--#UNIONS REMOVE DUPLICATES use UNION ALL TO GET EVERYTHING 

--SELECT * 
--FROM EmployeeDemo
--UNION ALL
--SELECT * 
--FROM Warehouse

--SELECT EmployeeID, FirstName, Age
--FROM EmployeeDemo
--UNION
--SELECT EmployeeID, JobTitle, Salary 
--FROM EmployeeSalary
--ORDER BY EmployeeID
--#This works because datatypes are similar and has same amount of columns but would not 
--Want to do this for these tables 

--#####CASE STATEMNETS ##########

--SELECT FirstName, LastName, Age,
--CASE
--	WHEN Age > 30 THEN 'Old'
--	WHEN Age BETWEEN 27 AND 30 THEN 'Young'
--	ELSE 'Baby'
--END
--FROM EmployeeDemo
--WHERE AGE IS NOT NULL
--ORDER BY AGE DESC


--SELECT FirstName, LastName, Age,
--CASE
--	WHEN Age > 30 THEN 'Old'
--	ELSE 'Baby'
--END
--FROM EmployeeDemo
--WHERE AGE IS NOT NULL
--ORDER BY AGE DESC

--When using case statements if similar statements first one will be returned 
--example:

--SELECT FirstName, LastName, Age,
--CASE
--	WHEN Age > 30 THEN 'Old'
--	WHEN Age = 38 THEN 'Stanley'
--	ELSE 'Baby'
--END
--FROM EmployeeDemo
--WHERE AGE IS NOT NULL
--ORDER BY AGE DESC

--Correct way if want to use case statment above 

--SELECT FirstName, LastName, Age,
--CASE
--	WHEN Age = 38 THEN 'Stanley'
--	WHEN Age > 30 THEN 'Old'
--	ELSE 'Baby'
--END
--FROM EmployeeDemo
--WHERE AGE IS NOT NULL
--ORDER BY AGE DESC

--SELECT FirstName, LastName, JobTitle, Salary,
--CASE
--	WHEN JobTitle = 'Salesman' THEN Salary +( Salary *.10)
--	WHEN JobTitle = 'Accountant' THEN Salary + (Salary *.05)
--	WHEN JobTitle = 'HR' THEN Salary + (Salary * .000001)
--	ELSE Salary + (Salary *.03)
--END AS SalaryAfterRaise
--FROM EmployeeDemo
--JOIN EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID 

--####HAVING CLAUSE#####

--SELECT JobTitle, AVG(Salary)
--FROM EmployeeDemo
--JOIN EmployeeSalary
--GROUP BY JobTitle
--HAVING AVG(Salary) > 45000
--ORDER BY AVG(Salary)

--SELECT JobTitle, COUNT(JobTitle)
--FROM EmployeeDemo
--JOIN EmployeeSalary
--GROUP BY JobTitle
--HAVING COUNT(JobTitle) >1

--SELECT * 
--FROM EmployeeDemo

--UPDATE [SQL Tutorial].dbo.EmployeeDemo
--SET EmployeeID = 1012
--WHERE FirstName = 'Holly' AND LastName = 'Flax'

--UPDATE [SQL Tutorial].dbo.EmployeeDemo
--SET Age = 31, Gender = 'Female'
--WHERE FirstName = 'Holly' AND LastName = 'Flax'

--DELETE FROM EmployeeDemo
--WHERE EmployeeID = 1005



--ALIASING 

--SELECT FirstName + ' ' + LastName AS FullName
--FROM EmployeeDemo

--SELECT AVG(Age) AS AvgAge
--FROM EmployeeDemo

--SELECT Demo.EmployeeID
--FROM EmployeeDemo AS Demo

--SELECT Demo.EmployeeID
--FROM EmployeeDemo AS Demo
--JOIN EmployeeSalary AS Sal
--	ON Demo.EmployeeID = Sal.EmployeeID

--SELECT Demo.EmployeeID, Demo.FirstName, Demo.LastName,
--	Sal.JobTitle, Ware.Age
--FROM EmployeeDemo AS Demo
--LEFT JOIN EmployeeSalary AS Sal
--	ON Demo.EmployeeID = Sal.EmployeeID
--LEFT JOIN Warehouse AS Ware
--	ON Demo.EmployeeID = Ware.EmployeeID

--PARTITION BY 
--GROUP BY reduces number of rows while partition divides up the info 

--SELECT FirstName, LastName, Gender, Salary,
--	COUNT(Gender) OVER (PARTITION BY Gender) as TotalGender
--FROM EmployeeDemo
--LEFT JOIN EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID 

--##Because using partition can isolate one column and maintain other columns 
--Differnce example below. 

--SELECT FirstName, LastName, Gender, Salary,
--	COUNT(Gender) 
--FROM EmployeeDemo
--LEFT JOIN EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID 
--GROUP BY FirstName, LastName, Gender, Salary


--SELECT Gender,COUNT(Gender) 
--FROM EmployeeDemo
--LEFT JOIN EmployeeSalary
--	ON EmployeeDemo.EmployeeID = EmployeeSalary.EmployeeID 
--GROUP BY Gender


--##CTE Complex Table Expression

--WITH CTE_Employee as (
--SELECT FirstName, LastName, Gender, Salary
--	, COUNT(Gender) OVER (PARTITION BY Gender) as TotalGender
--	, AVG(Salary) OVER (PARTITION BY Gender) as AvgSalary
--FROM EmployeeDemo AS Demo
--JOIN EmployeeSalary AS Sal
--	ON Demo.EmployeeID = Sal.EmployeeID
--WHERE Salary > '45000'
--)
--SELECT *
--FROM CTE_Employee

--WITH CTE_Employee as (
--SELECT FirstName, LastName, Gender, Salary
--	, COUNT(Gender) OVER (PARTITION BY Gender) as TotalGender
--	, AVG(Salary) OVER (PARTITION BY Gender) as AvgSalary
--FROM EmployeeDemo AS Demo
--JOIN EmployeeSalary AS Sal
--	ON Demo.EmployeeID = Sal.EmployeeID
--WHERE Salary > '45000'
--)
--SELECT FirstName, AvgSalary
--FROM CTE_Employee



--##TEMP TABLES##


--CREATE TABLE #temp_Employee (
--EmployeeID int, 
--JobtTitle varchar(100),
--Salary int
--)
--Select * 
--From #temp_Employee

----INSERT INTO #temp_Employee VALUES (
----'1001', 'HR', '45000'
----)

--INSERT INTO #temp_Employee
--SELECT * 
--FROM EmployeeSalary

--SELECT * 
--FROM #temp_Employee


--CREATE TABLE #temp_Employee2 (
--JobTitle varchar(50),
--EmployeesPerJob int, 
--AVGAge int, 
--AVGSalary int)

--INSERT INTO #temp_Employee2
--SELECT JobTitle, COUNT(JobTitle), AVG(Age), AVG(salary)
--FROM EmployeeDemo AS Demo
--JOIN EmployeeSalary AS Sal
--	ON Demo.EmployeeID = Sal.EmployeeID
--GROUP BY JobTitle

--SELECT * 
--FROM #temp_Employee2

--####If want to rerun query multiple times but get already exists error message can do:

--DROP TABLE IF EXISTS #temp_Employee2
--CREATE TABLE #temp_Employee2 (
--JobTitle varchar(50),
--EmployeesPerJob int, 
--AVGAge int, 
--AVGSalary int)

--INSERT INTO #temp_Employee2
--SELECT JobTitle, COUNT(JobTitle), AVG(Age), AVG(salary)
--FROM EmployeeDemo AS Demo
--JOIN EmployeeSalary AS Sal
--	ON Demo.EmployeeID = Sal.EmployeeID
--GROUP BY JobTitle

--SELECT * 
--FROM #temp_Employee2


--STRING FUNCTIONS ####

--CREATE TABLE EmployeeErrors (
--EmployeeID varchar(50),
--FirstName varchar(50),
--LastName varchar(50)
--)
--INSERT INTO EmployeeErrors VALUES
--('1001 ', 'Jimbo', 'Halbert'),
--(' 1002', 'Pamela', 'Beasely'),
--('1005', 'TOby', 'Flenderson - Fired')

--SELECT * 
--FROM EmployeeErrors

--###USING TRIM, LTRIM, RTRIM#####

--SELECT EmployeeID, TRIM(EmployeeID) as IDTrim
--FROM EmployeeErrors

--SELECT EmployeeID, RTRIM(EmployeeID) as IDTrim
--FROM EmployeeErrors


--SELECT EmployeeID, LTRIM(EmployeeID) as IDTrim
--FROM EmployeeErrors

--### REPLACE ####

--SELECT LastName, REPLACE(LastName, '- Fired','') as LASTNAMEFIXED
--FROM EmployeeErrors

--SUBSTRING ####

--SELECT SUBSTRING(FirstName,1,3)
--FROM EmployeeErrors

--##CAN USE Fuzzy matching eg if name Alex and Alexander and what both names use substring
--##SUBSTRING to find both EX Below
--##Use Gender, LastName, Age and DOB when fuzzy matching 

--SELECT err.FirstName, SUBSTRING(err.FirstName, 1,3), dem.FirstName, SUBSTRING(dem.FirstName, 1,3)
--FROM EmployeeErrors as err
--JOIN EmployeeDemo as dem
--	ON SUBSTRING(err.FirstName,1,3) = SUBSTRING(dem.FirstName,1,3)


--##UPPER AND LOWER ##


--SELECT FirstName, LOWER(FirstName)
--FROM EmployeeErrors

--SELECT FirstName, UPPER(FirstName)
--FROM EmployeeErrors


--##STORED PROCEDURES ##

--CREATE PROCEDURE TEST
--AS
--SELECT * 
--FROM EmployeeDemo

--EXEC dbo.TEST

--CREATE PROCEDURE Temp_Employee
--AS
--CREATE TABLE #temp_employee (
--JobTitle varchar(100),
--EmployeesPerJob int,
--AvgAge int,
--AvgSalary int,
--)

--Insert into #temp_employee
--SELECT JobTitle, Count(JobTitle), Avg(Age), AVG(Salary)
--FROM EmployeeDemo as Emp
--JOIN EmployeeSalary as Sal
--	ON Emp.EmployeeID = Sal.EmployeeID
--GROUP BY JobTitle

--SELECT * 
--FROM #temp_employee

--EXEC Temp_Employee



--Subquery in Select

--SELECT EmployeeID, Salary, (SELECT AVG (Salary) FROM EmployeeSalary) AS ALLAVGSAL
--FROM EmployeeSalary

--SELECT EmployeeID, Salary, AVG(Salary) over () as AVGSAL
--FROM EmployeeSalary



--Subquery in FROM statment (usually slow does not recommend)

--SELECT a.EmployeeID, AVGSAL
--FROM (SELECT EmployeeID, Salary, AVG(Salary) over () as AVGSAL
--FROM EmployeeSalary) a

--Subquery in Where
SELECT EmployeeID, JobTitle, Salary
FROM EmployeeSalary
WHERE EmployeeID in (
	SELECT EmployeeID
	FROM EmployeeDemo
	WHERE Age > 30)




