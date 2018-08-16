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


double mins[2];

// Functions

rnd::discrete_distribution clientProbs(json param, string clientType) {
	int numSps = param[clientType].size();
	rnd::discrete_distribution SpProb(numSps);
	for (json::iterator itSpClient = param[clientType].begin(); 
		itSpClient != param[clientType].end(); ++itSpClient) {
		SpProb[distance(param[clientType].begin(), itSpClient)] = 
			itSpClient->at("relAbun");
	}
	return (SpProb);
}

void draw(client trainingSet[], json param, 
	rnd::discrete_distribution visitSpProb,
	rnd::discrete_distribution residSpProb) {
	double cumProbs[3] = { param["ResProb"].get<double>(),
		(param["ResProb"].get<double>() + 	
			param["VisProb"].get<double>()),	1 };
	double rndNum;
	json::iterator itSpsVis = param["visitors"].begin();
	json::iterator itSpsRes = param["residents"].begin();
	for (int i = 0; i < param["totRounds"] * 2; i++) {
		rndNum = rnd::uniform();
		if (rndNum < cumProbs[0]) {
			string chosenSp = "Sp";
			chosenSp.append(itos(residSpProb.sample() + 1));
			trainingSet[i] = client(resident,param["residents"][chosenSp]["means"],
				param["residents"][chosenSp]["sds"],
				mins, param["residents"][chosenSp]["probs"],
				param["ResReward"].get<double>(),chosenSp);
		}
		else if (rndNum < cumProbs[1]) { 
			string chosenSp = "Sp";
			chosenSp.append(itos(visitSpProb.sample()+1));
			trainingSet[i] = client(visitor, param["visitors"][chosenSp]["means"],
				param["visitors"][chosenSp]["sds"],
				mins, param["visitors"][chosenSp]["probs"],
				param["ResReward"].get<double>(), chosenSp);
		}
		else { 
			trainingSet[i] = client(); 
		}
	}
}

string create_filename(std::string filename, agent &individual, 
	nlohmann::json param) {
	// name the file with the parameter specifications
	filename.append("alph");
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

void initializeIndFile(ofstream &indOutput, agent &learner, 
	nlohmann::json param, bool DP) {
	std::string namedir = param["folder"];
	//"D:\\quinonesa\\Simulation\\functionAprox\\"; 
	std::string namedirDP = param["folder"];
	//"D:\\quinonesa\\Simulation\\functionAprox\\";
	// "C:\\Users\\quinonesa\\prelimResults\\functionAprox\\";
	// "H:\\Dropbox\\Neuchatel\\prelimResults\\functionAprox\\"; 
	// "E:\\Dropbox\\Neuchatel\\prelimResults\\Set_15\\IndTrain_equVal"
	std::string folder;
	if(DP) {
		folder = "\\DP";
		folder.append("_");	
	}
	else {
		folder = typeid(learner).name();
		folder.erase(0, 6).append("_");
		cout << folder << '\t' << learner.getLearnPar(alphaPar) << '\t';
		cout << learner.getLearnPar(gammaPar) << '\t';
		cout << learner.getLearnPar(tauPar) << '\t';
		cout << learner.getLearnPar(netaPar) << endl;
	}
	namedir.append(folder);
	string IndFile = create_filename(namedir, learner, param);
	indOutput.open(IndFile.c_str());
	if(DP) {
		indOutput << "Time" << '\t' << "Alpha" << '\t' << "Gamma" << '\t';
		indOutput << "Tau" << '\t' << "Neta" << '\t' << "Outbr" << '\t';
		indOutput << "RV.V" << '\t' << "RV.R" << '\t' << "V0.V" << '\t'; 
		indOutput << "V0.0" << '\t' << "R0.R" << '\t' << "R0.0" << '\t';
		indOutput << "VV.V" << '\t' << "RR.R" << '\t' << "OO.O" << '\t';
		indOutput << endl;
	}
	else {
		indOutput << "Training" << '\t' << "Age" << '\t' << "Alpha" << '\t';
		indOutput << "Gamma" << '\t' << "Tau" << '\t' << "Neta" << '\t';
		indOutput << "Outbr" << '\t' << "Current.Reward" << '\t';
		indOutput << "Cum.Reward" << '\t' << "Neg.Reward" << '\t';
		indOutput << "value_choice" << '\t' << "Type_choice" << '\t';
		indOutput << "Height_choice" << '\t' << "Length_choice" << '\t';
		indOutput << "redMain_choice" << '\t' << "greenMain_choice" << '\t';
		indOutput << "blueMain_choice" << '\t' << "redSec_choice" << '\t';
		indOutput << "greenSec_choice" << '\t' << "blueSec_choice" << '\t';
		indOutput << "secCol_choice" << '\t' << "strip_choice" << '\t';
		indOutput << "dots_choice" << '\t' << "value_discard" << '\t';
		indOutput << "Type_discard" << '\t' << "Height_discard" << '\t';
		indOutput << "Length_discard" << '\t' << "redMain_discard" << '\t';
		indOutput << "greenMain_discard" << '\t' << "blueMain_discard" << '\t';
		indOutput << "redSec_discard" << '\t' << "greenSec_discard" << '\t';
		indOutput << "blueSec_discard" << '\t' << "secCol_discard" << '\t';
		indOutput << "strip_discard" << '\t' << "dots_discard" << '\t';

		if (learner.numEst > 11) {
			if (learner.numEst > 23) {
				indOutput << "Height_1_0" << '\t' << "Length_1_0" << '\t' << "redMain_1_0" << '\t';
				indOutput << "greenMain_1_0" << '\t' << "blueMain_1_0" << '\t' << "redSec_1_0" << '\t';
				indOutput << "greenSec_1_0" << '\t' << "blueSec_1_0" << '\t' << "secCol_1_0" << '\t';
				indOutput << "strip_1_0" << '\t' << "dots_1_0" << '\t' << "Height_2_0" << '\t';
				indOutput << "Length_2_0" << '\t' << "redMain_2_0" << '\t' << "greenMain_2_0" << '\t';
				indOutput << "blueMain_2_0" << '\t' << "redSec_2_0" << '\t' << "greenSec_2_0" << '\t';
				indOutput << "blueSec_2_0" << '\t' << "secCol_2_0" << '\t' << "strip_2_0" << '\t';
				indOutput << "dots_2_0" << '\t' << "Height_1_1" << '\t' << "Length_1_1" << '\t';
				indOutput << "redMain_1_1" << '\t' << "greenMain_1_1" << '\t' << "blueMain_1_1" << '\t';
				indOutput << "redSec_1_1" << '\t' << "greenSec_1_1" << '\t' << "blueSec_1_1" << '\t';
				indOutput << "secCol_1_1" << '\t' << "strip_1_1" << '\t' << "dots_1_1" << '\t';
				indOutput << "Height_2_1" << '\t' << "Length_2_1" << '\t' << "redMain_2_1" << '\t';
				indOutput << "greenMain_2_1" << '\t' << "blueMain_2_1" << '\t';
				indOutput << "redSec_2_1" << '\t' << "greenSec_2_1" << '\t' << "blueSec_2_1" << '\t';
				indOutput << "secCol_2_1" << '\t' << "strip_2_1" << '\t' << "dots_2_1" << '\t';
			}
			else {
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
		else {
			indOutput << "Height_1" << '\t' << "Length_1" << '\t' << "redMain_1" << '\t';
			indOutput << "greenMain_1" << '\t' << "blueMain_1" << '\t' << "redSec_1" << '\t';
			indOutput << "greenSec_1" << '\t' << "blueSec_1" << '\t' << "secCol_1" << '\t';
			indOutput << "strip_1" << '\t' << "dots_1" << '\t';
		}
		indOutput << endl;
	}
}

int _tmain(int argc, _TCHAR* argv[]) {

	ifstream input(argv[1]);
	//ifstream input("D:\\quinonesa\\learning_models_c++\\functionAprox\\test.json");
	//ifstream input("D:\\quinonesa\\Simulation\\functionAprox\\out_0\\parameters.json");
	if (input.fail()) { cout << "JSON file failed" << endl; }
	json param = nlohmann::json::parse(input);

	/*json param;

	param["totRounds"] = 10;
	param["ResReward"] = 10;
	param["VisReward"] = 10;
	param["ResProb"] = 0.2;
	param["VisProb"] = 0.2;
	param["ResProbLeav"] = 0;
	param["VisProbLeav"] = 1;
	param["negativeRew"] = -10;
	param["experiment"] = false;
	param["inbr"] = 0;
	param["outbr"] = 0;
	param["trainingRep"] = 1;
	param["alphaT"] = 1e-005;
	param["printGen"] = 1;
	param["seed"] = 1;
	param["gammaRange"] = { 0, 0.8 };
	param["tauRange"] = { 5, 10 },
		param["netaRange"] = { 0, 0.5 };
	param["mins"] = { 10, 10 };
	param["folder"] = "S:/quinonesa/Simulations/functionAprox/General/mHeightC30_/";
	param["visitors"]["Sp1"]["means"] = { 30, 20, 40, 40, 40, 40, 40, 40 };
	param["visitors"]["Sp1"]["sds"] = { 3, 3, 3, 3, 3, 3, 3, 3 };
	param["visitors"]["Sp1"]["probs"] = { 1, 1, 1 };
	param["visitors"]["Sp1"]["relAbun"] = 1;
	param["residents"]["Sp1"]["means"] = { 20, 30, 40, 40, 40, 40, 40, 40 };
	param["residents"]["Sp1"]["sds"] = { 3, 3, 3, 3, 3, 3, 3, 3 };
	param["residents"]["Sp1"]["probs"] = { 0, 1, 1 };
	param["residents"]["Sp1"]["relAbun"] = 1;*/
	
	const int numlearn = 2;
	
	mins[0] = param["mins"][0], mins[1] = param["mins"][1];

	rnd::set_seed(param["seed"].get<int>());
	rnd::discrete_distribution residSpProbs = clientProbs(param, "residents");
	rnd::discrete_distribution visitSpProbs = clientProbs(param, "visitors");

	client *clientSet;
	clientSet = new client[param["totRounds"].get<int>() * 2];
	int idClientSet;

	agent *learners[numlearn];

	cout << param["folder"] << endl;
	for (json::iterator itn = param["netaRange"].begin(); 
		itn != param["netaRange"].end(); ++itn) {
		for (json::iterator itg = param["gammaRange"].begin(); 
			itg != param["gammaRange"].end(); ++itg) {
			for (json::iterator itt = param["tauRange"].begin(); 
				itt != param["tauRange"].end(); ++itt) {
				ofstream printTest;
				ofstream DPprint;
				learners[0] = new FIATy2(param["alphaT"].get<double>(),
					*itg, *itt, *itn);
				learners[1] = new PIATy2(param["alphaT"].get<double>(),
					*itg, *itt, *itn);
				for (int k = 0; k < numlearn; ++k) {
					initializeIndFile(printTest, *learners[k], param,0);
					for (int i = 0; i < param["trainingRep"].get<int>(); 
						i++) {
						draw(clientSet, param, visitSpProbs,residSpProbs);
						idClientSet = 0;
						for (int j = 0; j < param["totRounds"].get<int>(); j++) {
							cout << "Round= " << j << endl;
							learners[k]->act(clientSet, idClientSet, param,
								visitSpProbs, residSpProbs);
							learners[k]->updateDerived();
							if (j % param["printGen"].get<int>() == 0) {
								learners[k]->printIndData(printTest, i);
							}
						}
						learners[k]->rebirth();
					}
					printTest.close();
					if (k == 0) {
						initializeIndFile(DPprint, *learners[0], param, 1);
						learners[k]->DPupdate(param["ResProb"].get<double>(), 
							param["VisProb"].get<double>(), 
							param["VisProbLeav"].get<double>(), 
							param["ResProbLeav"].get<double>(), 
							param["outbr"].get<double>(),
							param["ResReward"].get<double>(), 
							param["VisReward"].get<double>(), 
							param["negativeRew"].get<double>(), DPprint, 
							param["experiment"].get<bool>());
						DPprint.close();
					}					
					delete learners[k];
				}

			}
		}
	}

	delete[] clientSet;
	
	//wait_for_return();

	return 0;
}
