-- MySQL dump 10.13  Distrib 5.1.56, for redhat-linux-gnu (x86_64)
--
-- Host: localhost    Database: biodb
-- ------------------------------------------------------
-- Server version	5.1.56

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `class`
--

DROP TABLE IF EXISTS `class`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `class` (
  `id` int(11) NOT NULL,
  `name` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `class`
--

LOCK TABLES `class` WRITE;
/*!40000 ALTER TABLE `class` DISABLE KEYS */;
INSERT INTO `class` VALUES (1,'Bacteria'),(2,'Fungi'),(3,'Roundworms'),(4,'Insects'),(5,'Mammals'),(6,'Aliens');
/*!40000 ALTER TABLE `class` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `gene`
--

DROP TABLE IF EXISTS `gene`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `gene` (
  `id` int(11) NOT NULL,
  `mo_id` int(11) NOT NULL,
  `acc` varchar(45) DEFAULT NULL,
  `descr` varchar(100) DEFAULT NULL,
  `len` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_gene_modorg1` (`mo_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `gene`
--

LOCK TABLES `gene` WRITE;
/*!40000 ALTER TABLE `gene` DISABLE KEYS */;
INSERT INTO `gene` VALUES (1,9,'NM_000558','Homo sapiens hemoglobin, alpha 1 (HBA1), mRNA',576),(2,9,'NM_000517','Homo sapiens hemoglobin, alpha 2 (HBA2), mRNA',622),(3,9,'NM_000518','Homo sapiens hemoglobin, beta (HBB), mRNA',626),(4,9,'NM_000519','Homo sapiens hemoglobin, delta (HBD), mRNA',774),(5,6,'NM_008220','Mus musculus hemoglobin, beta adult major chain (Hbb-b1), mRNA',626),(6,6,'NM_008221','Mus musculus hemoglobin Y, beta-like embryonic chain (Hbb-y), mRNA',619),(7,6,'NM_008219','Mus musculus hemoglobin Z, beta-like embryonic chain (Hbb-bh1)',610),(8,7,'NM_033234','Rattus norvegicus hemoglobin, beta (Hbb), mRNA',620),(9,7,'NM_001172845','Rattus norvegicus hemoglobin, zeta (Hbz), mRNA',589);
/*!40000 ALTER TABLE `gene` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `modorg`
--

DROP TABLE IF EXISTS `modorg`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `modorg` (
  `id` int(11) NOT NULL,
  `class_id` int(11) NOT NULL,
  `genus` varchar(45) DEFAULT NULL,
  `species` varchar(45) DEFAULT NULL,
  `nchr` int(11) DEFAULT NULL,
  `gsize` double DEFAULT NULL,
  `draft` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_modorg_class` (`class_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `modorg`
--

LOCK TABLES `modorg` WRITE;
/*!40000 ALTER TABLE `modorg` DISABLE KEYS */;
INSERT INTO `modorg` VALUES (1,1,'Escherichia','coli',1,4.639,'1997-09-05 00:00:00'),(2,2,'Saccharomyces','cerevisiae',16,0.672,'1996-10-25 00:00:00'),(3,3,'Caenorhabditis','elegans',6,100,'1998-12-16 00:00:00'),(4,4,'Drosophila','melanogester',4,180,'2000-03-25 00:00:00'),(5,4,'Apis','melifera',16,218,'2003-12-19 00:00:00'),(6,5,'Mus','musculus',21,2651,'2010-09-23 00:00:00'),(7,5,'Rattus','norvegicus',21,6597,'2004-04-02 00:00:00'),(8,5,'Gorilla','gorilla',24,3076,'2008-11-03 00:00:00'),(9,5,'Homo','sapiens',23,3038,'2007-05-22 00:00:00'),(10,6,'Alienus','area51us',NULL,NULL,NULL);
/*!40000 ALTER TABLE `modorg` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2011-06-21  4:50:15
