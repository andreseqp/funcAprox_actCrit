// Simple_Sarsa2.cpp : Defines the entry point for the console application.
//
/*==============================================================================================================
Simple_Sarsa2.cpp
===============================================================================================================

This is the a new version of the algorithm that allows a learning agent to learn to make desicions in the
context of the market experiment in the context of the cleaner wrasse mutualism.
The difference between this implementation and the main algorithm from 'Simple_Sarsa.cpp' is that in here agents
only estimate the state-action pairs related to the market experiment; while originally an agent also estimated
values relates to the silly option of food agaist nothing. A detailed description of the model can be found in
"E:\Dropbox\Neuchatel\Cleaners_learning_model\\model_description_06_2_2017.pdf".
The model uses the Sarsa algorithm, from the Time Difference (TD) methods from reinforcement learning, to teach
an agent to solve the market expertiment that cleaners face in experimental trials. In the market experiment
individuals are offered two options of clients to clean. This options can be a visitor, a resident, or the
absence of clients. The difference between the two types of clients is that visitors leave the cleaning station
when they are not served, while residents wait; thus, are available in the next time step.




Written by:

Andrés E. Quiñones
Posdoctoral researcher
Behavioural Ecology Group
Institute of Biology
University of Neuchâtel
Switzerland

Start date:
6 February 2017

Last edit date:
15 August 2017


=============================================================================================================*/

#include <stdio.h>
#include <cstdlib>
#include <math.h>
#include <iostream>
#include <fstream>
#include <vector>
// Random number generator
#include "M:\\Routines\\C++\\RandomNumbers\\random.h" 
//H for house pc, E for laptop, M for Office
#include "M:\\Routines\\C++\\RandomNumbers\\stdafx.h"
// Classes
#include "cleaner.h"
#include "Client.h"
// Jason parser
#include "D:\\quinonesa\\Dropbox\C++\\json.hpp"       
// Header for reading and using JSON files see https://github.com/nlohmann/json

#define GET_VARIABLE_NAME(Variable) (#Variable)

using namespace std;
using json = nlohmann::json;

double visitMeans[8], visitSds[8], visitProbs[3];
double residMeans[8], residSds[8], residProbs[3];
double mins[2];
double ResReward, VisReward;


// Functions

void draw(client trainingSet[], int rounds, double &probRes, double &probVis)
{
	double cumProbs[3] = { probRes, probRes + probVis, 1 };
	double rndNum;
	for (int i = 0; i < rounds * 2; i++)
	{
		rndNum = rnd::uniform();
		if (rndNum < cumProbs[0]) 
		{ 
			trainingSet[i] = client(resident,residMeans,residSds,mins,residProbs,ResReward); 
		}
		else if (rndNum < cumProbs[1]) 
		{ 
			trainingSet[i] = client(visitor, visitMeans, visitSds, mins, visitProbs, VisReward); 
		}
		else 
		{ 
			trainingSet[i] = client(absence,residMeans,residSds,mins,residProbs,ResReward); 
		}
	}
}

std::string itos(int j)				// turns int into string
{
	std::stringstream s;
	s << j;
	return s.str();
}

std::string douts(double j)			// turns double into string
{
	std::stringstream s;
	s << j;
	return s.str();
}

string create_filename(std::string filename, agent &individual, nlohmann::json param)
{
	// name the file with the parameter specifications
	filename.append("_alph");
	filename.append(douts(individual.getLearnPar(alphaPar)));
	filename.append("_gamma");
	filename.append(douts(individual.getLearnPar(gammaPar)));
	filename.append("_tau");
	filename.append(douts(individual.getLearnPar(tauPar)));
	filename.append("_neta");
	filename.append(douts(individual.getLearnPar(netaPar)));
	filename.append("_outb");
	filename.append(douts(param["outbr"]));
	filename.append("_seed");
	filename.append(itos(param["seed"]));
	filename.append(".txt");
	return(filename);
}

void initializeIndFile(ofstream &indOutput, ofstream &DPOutput, agent &learner, nlohmann::json param)
{
	std::string namedir = "D:\\quinonesa\\Simulation\\functionAprox\\"; 
	std::string namedirDP = "D:\\quinonesa\\Simulation\\functionAprox\\";
	// "C:\\Users\\quinonesa\\prelimResults\\functionAprox\\";
	// "H:\\Dropbox\\Neuchatel\\prelimResults\\functionAprox\\"; 
	// "E:\\Dropbox\\Neuchatel\\prelimResults\\Set_15\\IndTrain_equVal"
	std::string folder = typeid(learner).name();
	std::string DPfolder = folder;
	folder.erase(0, 6).append("\\IndTrain");
	DPfolder.erase(0, 6).append("\\DP");
	namedir.append(folder);
	namedirDP.append(DPfolder);
	cout << namedir << '\t' << learner.getLearnPar(alphaPar) << '\t';
	cout << learner.getLearnPar(gammaPar) << '\t' << learner.getLearnPar(tauPar) << '\t';
	cout << learner.getLearnPar(netaPar) << endl;
	string IndFile = create_filename(namedir, learner, param);
	string DPfile = create_filename(namedirDP, learner, param);
	indOutput.open(IndFile.c_str());
	DPOutput.open(DPfile.c_str());
	indOutput << "Training" << '\t' << "Age" << '\t' << "Alpha" << '\t';
	indOutput << "Gamma" << '\t' << "Tau" << '\t' << "Neta" << '\t';
	indOutput << "Outbr" << '\t' << "Current.Reward" << '\t' << "Cum.Reward";
	indOutput << '\t' << "Neg.Reward" << '\t';
	indOutput << "value_choice" << '\t' << "Type_choice" << '\t' << "Height_choice" << '\t';
	indOutput << "Length_choice" << '\t' << "redMain_choice" << '\t' << "greenMain_choice" << '\t';
	indOutput << "blueMain_choice" << '\t' << "redSec_choice" << '\t' << "greenSec_choice" << '\t';
	indOutput << "blueSec_choice" << '\t' << "secCol_choice" << '\t' << "strip_choice" << '\t';
	indOutput << "dots_choice" << '\t' << "value_discard" << '\t' << "Type_discard" << '\t';
	indOutput << "Height_discard" << '\t' << "Length_discard" << '\t' << "redMain_discard" << '\t';
	indOutput << "greenMain_discard" << '\t' << "blueMain_discard" << '\t' << "redSec_discard" << '\t';
	indOutput << "greenSec_discard" << '\t' << "blueSec_discard" << '\t' << "secCol_discard" << '\t';
	indOutput << "strip_discard" << '\t' << "dots_discard" << '\t';

	if (learner.numEst > 11)
	{
		if (learner.numEst > 23)
		{
			indOutput << "Height_1_0" << '\t' << "Length_1_0" << '\t' << "redMain_1_0" << '\t';
			indOutput << "greenMain_1_0" << '\t' << "blueMain_1_0" << '\t' << "redSec_1_0" << '\t';
			indOutput << "greenSec_1_0" << '\t' << "blueSec_1_0" << '\t' << "secCol_1_0" << '\t';
			indOutput << "strip_1_0" << '\t' << "dots_1_0" << '\t' << "Height_2_0" << '\t';
			indOutput << "Length_2_0" << '\t' << "redMain_2_0" << '\t' << "greenMain_2_0" << '\t';
			indOutput << "blueMain_2_0" << '\t' << "redSec_2_0" << '\t' << "greenSec_2_0" << '\t';
			indOutput << "blueSec_2_0" << '\t' << "secCol_2_0" << '\t' << "strip_2_0" << '\t';
			indOutput << "dots_2_0" << '\t' << "Height_1_1" << '\t' << "Length_1_1" << '\t' ;
			indOutput << "redMain_1_1" << '\t' << "greenMain_1_1" << '\t' << "blueMain_1_1" << '\t';
			indOutput << "redSec_1_1" << '\t' << "greenSec_1_1" << '\t' << "blueSec_1_1" << '\t';
			indOutput << "secCol_1_1" << '\t' << "strip_1_1" << '\t' << "dots_1_1" << '\t';
			indOutput << "Height_2_1" << '\t' << "Length_2_1" << '\t' << "redMain_2_1" << '\t';
			indOutput << "greenMain_2_1" << '\t' << "blueMain_2_1" << '\t';
			indOutput << "redSec_2_1" << '\t' << "greenSec_2_1" << '\t' << "blueSec_2_1" << '\t';
			indOutput << "secCol_2_1" << '\t' << "strip_2_1" << '\t' << "dots_2_1" << '\t';
		}
		else
		{
			indOutput << "Height_1" << '\t' << "Length_1" << '\t' << "redMain_1" << '\t';
			indOutput << "greenMain_1" << '\t' << "blueMain_1" << '\t';
			indOutput << "redSec_1" << '\t' << "greenSec_1" << '\t' << "blueSec_1" << '\t';
			indOutput << "secCol_1" << '\t' << "strip_1" << '\t' << "dots_1" << '\t';
			indOutput << "Height_2" << '\t' << "Length_2" << '\t' << "redMain_2" << '\t';
			indOutput << "greenMain_2" << '\t' << "blueMain_2" << '\t' << "redSec_2" << '\t';
			indOutput << "greenSec_2" << '\t' << "blueSec_2" << '\t' << "secCol_2" << '\t';
			indOutput << "strip_2" << '\t' << "dots_2" << '\t' << "featChoice";
		}
	}
	else
	{
		indOutput << "Height_1" << '\t' << "Length_1" << '\t' << "redMain_1" << '\t';
		indOutput << "greenMain_1" << '\t' << "blueMain_1" << '\t' << "redSec_1" << '\t';
		indOutput << "greenSec_1" << '\t' << "blueSec_1" << '\t' << "secCol_1" << '\t';
		indOutput << "strip_1" << '\t' << "dots_1" << '\t';
	}
	indOutput << endl;
	DPOutput << "Time" << '\t' << "Alpha" << '\t' << "Gamma" << '\t' << "Tau" << '\t' << "Neta" << '\t' << "Outbr" << '\t';
	DPOutput << "RV.V" << '\t' << "RV.R" << '\t' << "V0.V" << '\t' << "V0.0" << '\t' << "R0.R" << '\t' << "R0.0" << '\t' << "VV.V" << '\t' << "RR.R" << '\t' << "OO.O" << '\t';
	DPOutput << endl;
}

int _tmain(int argc, _TCHAR* argv[])
{
	ifstream input(argv[1]);
	//ifstream input("D:\\quinonesa\\learning_models_c++\\functionAprox\\test.json");
	if (input.fail()) { cout << "JSON file failed" << endl; }
	json param = nlohmann::json::parse(input);

	int const totRounds = param["totRounds"];
	double ResReward = param["ResReward"];
	double VisReward = ResReward;
	double ResProb = param["ResProb"];
	double VisProb = ResProb;
	double ResProbLeav = param["ResProbLeav"];
	double VisProbLeav = param["VisProbLeav"];
	double negativeRew = param["negativeRew"];
	bool experiment = param["experiment"];
	double inbr = param["inbr"];
	double outbr = param["outbr"];
	int trainingRep = param["trainingRep"];
	double alphaT = param["alphaT"];
	const int numlearn = 2;
	int printGen = param["printGen"];
	int seed = param["seed"];

	json::iterator vsd = param["visitors"]["Sp1"]["sds"].begin();
	json::iterator rm = param["residents"]["Sp1"]["means"].begin();
	json::iterator rsds = param["residents"]["Sp1"]["sds"].begin();
	for (json::iterator vm = param["visitors"]["Sp1"]["means"].begin(); 
		vm != param["visitors"]["Sp1"]["means"].end(); vm++)
	{
		visitMeans[vm-param["visitors"]["Sp1"]["means"].begin()] = *vm;
		visitSds[vsd-param["visitors"]["Sp1"]["sds"].begin()] = *vsd, ++vsd;
		residMeans[rm-param["residents"]["Sp1"]["means"].begin()] = *rm, ++rm;
		residSds[rsds-param["residents"]["Sp1"]["sds"].begin()] = *rsds, ++rsds;
	}

	json::iterator vp = param["visitors"]["Sp1"]["probs"].begin();
	for (json::iterator rp = param["residents"]["Sp1"]["probs"].begin();
		rp != param["residents"]["Sp1"]["probs"].end(); ++rp)
	{
		residProbs[rp - param["residents"]["Sp1"]["probs"].begin()] = *rp;
		visitProbs[vp - param["visitors"]["Sp1"]["probs"].begin()] = *vp, ++vp;
	}

	mins[0] = param["mins"][0], mins[1] = param["mins"][1];
	 /*
	for (size_t i = 0; i < 8; i++)
	{
		visitMeans[i] = 40;
		visitSds[i] = 3;
		residMeans[i] = 40;
		residSds[i] = 3;
		if (i < 3){
			visitProbs[i] = 1;
			residProbs[i] = 1;
		}
	}
	visitMeans[0] = 30, residMeans[0] = 20;
	visitMeans[1] = 20, residMeans[1] = 30;
	residProbs[0] = 0;
	mins[0] = 10, mins[1] = 10;
	int totRounds = 100000;
	ResReward = 10;
	VisReward = 10;
	double ResProb = 0.2;
	double VisProb = 0.2;
	double ResProbLeav = 0;
	double VisProbLeav = 1;
	double negativeRew = -10;
	bool experiment = 0;
	double inbr = 0.0;
	double outbr = 0;
	int const trainingRep = 15;//30
	double alphaT = 0.000001;
	const int numlearn = 2;
	int printGen = 10;

	double gammaT;

	double gammaRange[3] = { 0, 0.5, 0.8 };

	double tauT;

	double tauRange[3] = { 1, 2, 5};
	
	double netaT = 0;

	double netaRange[1] = { 0.5 };


	int seed = 12;*/

	rnd::set_seed(seed);

	client *clientSet;
	clientSet = new client[totRounds * 2];
	int idClientSet;

	agent *learners[numlearn];

	
	for (json::iterator itn = param["netaRange"].begin(); itn != param["netaRange"].end(); ++itn) 
	{
		for (json::iterator itg = param["gammaRange"].begin(); itg != param["gammaRange"].end(); ++itg)
		{
			for (json::iterator itt = param["tauRange"].begin(); itt != param["tauRange"].end(); ++itt) 
			{
				ofstream printTest;
				ofstream DPprint;
				learners[0] = new StatPosTyp1(alphaT, *itg, *itt, *itn);
				learners[1] = new ActPosTy1(alphaT, *itg, *itt, *itn);

				for (int k = 0; k < numlearn; ++k)  //numlearn
				{
					initializeIndFile(printTest, DPprint, *learners[k], param);
					for (int i = 0; i < trainingRep; i++)
					{
						draw(clientSet, totRounds, ResProb, VisProb);
						idClientSet = 0;
						for (int j = 0; j < totRounds; j++)
						{
							learners[k]->act(clientSet, idClientSet, VisProbLeav, ResProbLeav, 
								VisReward, ResReward, inbr, outbr, negativeRew, experiment);
							learners[k]->updateDerived();
							if (j%printGen == 0)
							{
								learners[k]->printIndData(printTest, i, outbr);
							}
						}
						learners[k]->rebirth();
					}
					printTest.close();
					learners[k]->DPupdate(ResProb, VisProb, VisProbLeav, ResProbLeav, outbr, 
						ResReward, VisReward, negativeRew, DPprint, experiment);
					DPprint.close();
					delete learners[k];
				}

			}
		}
	}

	delete[] clientSet;
	
	wait_for_return();

	return 0;
}
