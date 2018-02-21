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
// Random number generator
#include "M:\\Routines\\C++\\RandomNumbers\\random.h" //H for house pc, E for laptop, M for Office
#include "M:\\Routines\\C++\\RandomNumbers\\stdafx.h"
// Classes
#include "cleaner.h"
#include "Client.h"

#define GET_VARIABLE_NAME(Variable) (#Variable)

using namespace std;

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
		if (rndNum < cumProbs[0]) { trainingSet[i] = client(resident,residMeans,residSds,mins,residProbs,ResReward); }
		else if (rndNum < cumProbs[1]) { trainingSet[i] = client(visitor, visitMeans, visitSds, mins, visitProbs, VisReward); }
		else { trainingSet[i] = client(absence,residMeans,residSds,mins,residProbs,ResReward); }
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

string create_filename(std::string filename, agent &individual, int &seed, double &bias)
{
	// name the file with the parameter specifications
	filename.append("_alph");
	filename.append(douts(individual.getLearnPar(alphaPar)));
	filename.append("_gamma");
	filename.append(douts(individual.getLearnPar(gammaPar)));
	filename.append("_tau");
	filename.append(douts(individual.getLearnPar(tauPar)));
	filename.append("_bias");
	filename.append(douts(bias));
	filename.append("_seed");
	filename.append(itos(seed));
	filename.append(".txt");
	return(filename);
}

void initializeIndFile(ofstream &indOutput, agent &learner, int &seed,double &bias)
{
	std::string namedir = "C:\\Users\\quinonesa\\prelimResults\\functionAprox\\";// "H:\\Dropbox\\Neuchatel\\prelimResults\\functionAprox\\"; // "E:\\Dropbox\\Neuchatel\\prelimResults\\Set_15\\IndTrain_equVal"
	std::string folder = typeid(learner).name();
	folder.erase(0, 6).append("\\IndTrain_test");
	namedir.append(folder);
	cout << namedir << endl;
	string IndFile = create_filename(namedir, learner, seed, bias);
	indOutput.open(IndFile.c_str());
	indOutput << "Training" << '\t' << "Age" << '\t' << "Alpha" << '\t' << "Gamma" << '\t' << "Tau" << '\t' << "Bias" << '\t' << "Current.Reward" << '\t' << "Cum.Reward" << '\t' << "Neg.Reward" << '\t';
	indOutput << "value_choice" << '\t' << "Type_choice" << '\t' << "Height_choice" << '\t' << "Length_choice" << '\t' << "redMain_choice" << '\t' << "greenMain_choice" << '\t' << "blueMain_choice" << '\t';
	indOutput << "redSec_choice" << '\t' << "greenSec_choice" << '\t' << "blueSec_choice" << '\t' << "secCol_choice" << '\t' << "strip_choice" << '\t' << "dots_choice" << '\t';
	indOutput << "value_discard" << '\t' << "Type_discard" << '\t' << "Height_discard" << '\t' << "Length_discard" << '\t' << "redMain_discard" << '\t' << "greenMain_discard" << '\t' << "blueMain_discard" << '\t';
	indOutput << "redSec_discard" << '\t' << "greenSec_discard" << '\t' << "blueSec_discard" << '\t' << "secCol_discard" << '\t' << "strip_discard" << '\t' << "dots_discard" << '\t';
	indOutput << "DPUpdate" << '\t';
	if (learner.numEst > 11)
	{
		if (learner.numEst > 23)
		{
			indOutput << "Height_1_0" << '\t' << "Length_1_0" << '\t' << "redMain_1_0" << '\t' << "greenMain_1_0" << '\t' << "blueMain_1_0" << '\t';
			indOutput << "redSec_1_0" << '\t' << "greenSec_1_0" << '\t' << "blueSec_1_0" << '\t' << "secCol_1_0" << '\t' << "strip_1_0" << '\t' << "dots_1_0" << '\t';
			indOutput << "Height_2_0" << '\t' << "Length_2_0" << '\t' << "redMain_2_0" << '\t' << "greenMain_2_0" << '\t' << "blueMain_2_0" << '\t';
			indOutput << "redSec_2_0" << '\t' << "greenSec_2_0" << '\t' << "blueSec_2_0" << '\t' << "secCol_2_0" << '\t' << "strip_2_0" << '\t' << "dots_2_0" << '\t';
			indOutput << "Height_1_1" << '\t' << "Length_1_1" << '\t' << "redMain_1_1" << '\t' << "greenMain_1_1" << '\t' << "blueMain_1_1" << '\t';
			indOutput << "redSec_1_1" << '\t' << "greenSec_1_1" << '\t' << "blueSec_1_1" << '\t' << "secCol_1_1" << '\t' << "strip_1_1" << '\t' << "dots_1_1" << '\t';
			indOutput << "Height_2_1" << '\t' << "Length_2_1" << '\t' << "redMain_2_1" << '\t' << "greenMain_2_1" << '\t' << "blueMain_2_1" << '\t';
			indOutput << "redSec_2_1" << '\t' << "greenSec_2_1" << '\t' << "blueSec_2_1" << '\t' << "secCol_2_1" << '\t' << "strip_2_1" << '\t' << "dots_2_1" << '\t';
		}
		else
		{
			indOutput << "Height_1" << '\t' << "Length_1" << '\t' << "redMain_1" << '\t' << "greenMain_1" << '\t' << "blueMain_1" << '\t';
			indOutput << "redSec_1" << '\t' << "greenSec_1" << '\t' << "blueSec_1" << '\t' << "secCol_1" << '\t' << "strip_1" << '\t' << "dots_1" << '\t';
			indOutput << "Height_2" << '\t' << "Length_2" << '\t' << "redMain_2" << '\t' << "greenMain_2" << '\t' << "blueMain_2" << '\t';
			indOutput << "redSec_2" << '\t' << "greenSec_2" << '\t' << "blueSec_2" << '\t' << "secCol_2" << '\t' << "strip_2" << '\t' << "dots_2" << '\t';
			indOutput << "featChoice";
		}
	}
	else
	{
		indOutput << "Height_1" << '\t' << "Length_1" << '\t' << "redMain_1" << '\t' << "greenMain_1" << '\t' << "blueMain_1" << '\t';
		indOutput << "redSec_1" << '\t' << "greenSec_1" << '\t' << "blueSec_1" << '\t' << "secCol_1" << '\t' << "strip_1" << '\t' << "dots_1" << '\t';
	}
	indOutput << endl;
}

int _tmain(int argc, _TCHAR* argv[])
{
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


	int seed = 12;
	rnd::set_seed(seed);

	client *clientSet;
	clientSet = new client[totRounds * 2];
	int idClientSet;

	agent *learners[numlearn];


	for (size_t k = 0; k < 3; k++)
	{
		for (size_t l = 0; l < 3; l++)
		{
			gammaT = gammaRange[k];
			tauT = tauRange[0];
			ofstream printTest;
			ofstream DPprint;
			
			learners[0] = new StatPosTyp1(alphaT, gammaT, tauT, netaT);
			learners[4] = new ActPosTy1(alphaT, gammaT, tauT, netaT);
			
			//learners[0] = new StatPosTyp2(alphaT, gammaT, tauT);
			
			for (int k=0;k<1;++k)  //numlearn
			{
				initializeIndFile(printTest, *learners[k], seed, outbr);
				for (int i = 0; i < trainingRep; i++)
				{
					draw(clientSet, totRounds, ResProb, VisProb);
					idClientSet = 0;
					for (int j = 0; j < totRounds; j++)
					{
						learners[k]->act(clientSet, idClientSet, VisProbLeav, ResProbLeav, VisReward, ResReward,inbr,outbr, negativeRew,experiment);
						learners[k]->updateDerived();
						if (j%printGen == 0)
						{
							learners[k]->printIndData(printTest, i, outbr);
						}
					}
					learners[k]->rebirth();
				}
				printTest.close();
				learners[k]->DPupdate(ResProb, VisProb, VisProbLeav, ResProbLeav, outbr, ResReward, VisReward,negativeRew, DPprint, experiment);
				DPprint.close();
				delete learners[k];
			}
			
		}
	}

	delete[] clientSet;
	
	wait_for_return();

	return 0;
}
