--SELECT * 
--FROM CovidDeaths$
--ORDER BY 3,4

--SELECT * 
--FROM CovidVaccinations$
--ORDER BY 3,4

--##SELECT DATA WE WILL BE USING##

--SELECT Location, Date,total_cases, new_cases, total_deaths, population
--FROM CovidDeaths$
--ORDER BY 1,2

--##Looking at Total Cases vs Total Deaths ###

--SELECT location, Date,total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPct
--FROM CovidDeaths$
--WHERE location like '%states%'
--ORDER BY 1,2

--SELECT location, Date,total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPct
--FROM CovidDeaths$
--WHERE location like '%states%'
--ORDER BY 1,2

--##Look @ total cases vs population##

--SELECT location, Date,total_cases, population, (total_cases/population)*100 as ContractionPct
--FROM CovidDeaths$
--WHERE location like '%states%'
--ORDER BY 1,2

--##Highest infection rates per country compared to pop###

--SELECT location, population, MAX(total_cases) as HighestInfectionCount, MAX((total_cases/population))*100 as pctinfected
--FROM CovidDeaths$
--GROUP BY population,location
--ORDER BY pctinfected DESC

--##COUNTRIES WITH HIGHEST DEATH COUNT PER POPULATION

--SELECT location, MAX(cast(total_deaths as int)) as totaldeathcount
--FROM CovidDeaths$
--GROUP BY location
--ORDER BY totaldeathcount DESC

--SELECT*
--FROM CovidDeaths$
--ORDER BY 3,4

--SELECT location, MAX(cast(total_deaths as int)) as totaldeathcount
--FROM CovidDeaths$
--WHERE continent IS NOT NULL
--GROUP BY location
--ORDER BY totaldeathcount DESC

--###BREAKDOWN BY CONTINENT 

--SELECT location, MAX(cast(total_deaths as int)) as totaldeathcount
--FROM CovidDeaths$
--WHERE continent IS NULL
--GROUP BY location
--ORDER BY totaldeathcount DESC

--SELECT continent,location, MAX(cast(total_deaths as int)) as totaldeathcount
--FROM CovidDeaths$
--WHERE continent IS NOT NULL
--GROUP BY continent, location
--ORDER BY continent, totaldeathcount DESC

--##BREAKDOWN GLOBAL NUMBERS###

--SELECT date, total_cases, total_deaths, (total_deaths/total_cases)*100 as PCT
--FROM CovidDeaths$
--WHERE continent IS NOT NULL
--GROUP BY date
--ORDER BY PCT
----## THIS DOESENT WORK NEED TO USE AGGREGATE FUNCTION##

--SELECT date, SUM(new_cases) as TOTCase, SUM(cast(new_deaths as int)) as TOTDeath, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as PCT
--FROM CovidDeaths$
--WHERE continent IS NOT NULL
--GROUP BY date
--ORDER BY 1,2


--SELECT SUM(new_cases) as TOTCase, SUM(cast(new_deaths as int)) as TOTDeath, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as PCT
--FROM CovidDeaths$
--WHERE continent IS NOT NULL
--ORDER BY 1,2

--##Total population vs VAX####

--SELECT * 
--FROM CovidDeaths$ as death
--JOIN CovidVaccinations$ as vax
--	ON death.location = vax.location and
--	death.date = vax.date

--SELECT death.continent, death.location, death.date, death.population, vax.new_vaccinations
--FROM CovidDeaths$ as death
--JOIN CovidVaccinations$ as vax
--	ON death.location = vax.location and
--	death.date = vax.date
--WHERE death.continent IS NOT NULL
--ORDER BY 2,3


--##ROLLING COUNT OF VAXES ####

--SELECT death.continent, death.location, death.date, death.population, vax.new_vaccinations
--, SUM(convert(int,vax.new_vaccinations )) OVER (PARTITION BY death.location ORDER BY death.location, 
--death.date) as RollingVaxCount
--FROM CovidDeaths$ as death
--JOIN CovidVaccinations$ as vax
--	ON death.location = vax.location and
--	death.date = vax.date
--WHERE death.continent IS NOT NULL
--ORDER BY 2,3


--##USING CTE TO FIGURE OUT % OF POPULATION VAX NUMBERS AS PEOPLE GET VAXXED#####

--WITH PopVsVax (continent, location, date, population, new_vaccinations, RollingVaxCount)
--as
--(SELECT death.continent, death.location, death.date, death.population, vax.new_vaccinations
--, SUM(convert(int,vax.new_vaccinations )) OVER (PARTITION BY death.location ORDER BY death.location, 
--death.date) as RollingVaxCount
--FROM CovidDeaths$ as death
--JOIN CovidVaccinations$ as vax
--	ON death.location = vax.location and
--	death.date = vax.date
--WHERE death.continent IS NOT NULL)

--SELECT * , (RollingVaxCount/population)*100
--FROM PopVsVax

--###OR TEMP TABLE ####


--DROP TABLE IF EXISTS #PercentPopVax
--CREATE TABLE #PercentPopVax (
--continent nvarchar(255), 
--location nvarchar(255), 
--date datetime,
--population numeric, 
--new_vaccinations numeric, 
--RollingVaxCount numeric)
--INSERT INTO #PercentPopVax
--SELECT death.continent, death.location, death.date, death.population, vax.new_vaccinations
--, SUM(convert(int,vax.new_vaccinations )) OVER (PARTITION BY death.location ORDER BY death.location, 
--death.date) as RollingVaxCount
--FROM CovidDeaths$ as death
--JOIN CovidVaccinations$ as vax
--	ON death.location = vax.location and
--	death.date = vax.date
--WHERE death.continent IS NOT NULL

--SELECT * , (RollingVaxCount/population)*100
--FROM #PercentPopVax


----###USING A VIEW TO STORE DATA FOR LATER VIZ###

--CREATE VIEW PCTPOPVAX as
--SELECT death.continent, death.location, death.date, death.population, vax.new_vaccinations
--, SUM(convert(int,vax.new_vaccinations )) OVER (PARTITION BY death.location ORDER BY death.location, 
--death.date) as RollingVaxCount
--FROM CovidDeaths$ as death
--JOIN CovidVaccinations$ as vax
--	ON death.location = vax.location and
--	death.date = vax.date
--WHERE death.continent IS NOT NULL

