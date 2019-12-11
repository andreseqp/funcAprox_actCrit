#pragma once
#ifndef CLEANER_H
#define CLEANER_H
#include "Client.h"

#include <fstream>
#include "C:\\Users\\a.quinones\\Dropbox\C++\\json.hpp"  


enum learPar { alphaCritPar, alphaActPar, gammaPar, netaPar };

// Global variables
// Client's characteristics
extern double mins[2];
extern double boolExtUpdate = 10;


class agent {
	// Learning agent
public:
	agent(double alphaCI, double alphaAI, double gammaI, bool netaI, 
		double InitVal);
	// constructor providing values for the learning parameters
	virtual ~agent();
	// destructor not really necessary
	void updateClient(int &countWeights, int clientId);
	// function that updates the value of state-action pairs accosrding to 
	// current reward and estimates of future values
	void act(client newOptions[], int &idNewOptions, nlohmann::json param,
		rnd::discrete_distribution visitSpProb,
		rnd::discrete_distribution residSpProb);
	// function where the agent takes the action, gets reward, 
	//see new state and chooses future action
	void printIndData(std::ofstream &learnSeries, int &seed);
	// prints individual data from the learning process
	double getLearnPar(learPar parameter);
	// function to access the learning parameters
	void checkChoice();
	// Check that the choice taken is among one of the options, 
	// otherwise trigger an error
	void rebirth(double InitVal);
	// Function to reset private variables in an individual
	void getExternalOptions(client newOptions[], int &idNewOptions, 
		nlohmann::json param,
		rnd::discrete_distribution visitSpProb,
		rnd::discrete_distribution residSpProb);
	// After unattended clinets leave or stay, get new clients
	void ObtainReward();
	double linearComClient(int clientid, int &countFeat,
		double featWeights[]);
	// Use linear combinations of features to calculate either value or preference
	double logist(double diffPref);
	// Use the preference to calculate the probability of choosing 
	// the first (0) client
	void getNewOptions(client newOptions[], int &idNewOptions, 
		nlohmann::json param, rnd::discrete_distribution visitSpProb,
		rnd::discrete_distribution residSpProb);
	void getExperimentalOptions(nlohmann::json param,
		rnd::discrete_distribution visitSpProb,
		rnd::discrete_distribution residSpProb);
	// function to obtain options as in an experimental trial
	client cleanOptionsT[2];		
	// current cleaning options time = t
	client cleanOptionsT1[2];
	// future cleaning options  time = t+1																											
	
	// virtual functions
	virtual double value() = 0;
	// Calculates the value estimation for the current state
	virtual double preference() = 0;
	// Calculates the preference for chosing the first (0) client.
	virtual void choice() = 0;
	virtual void updateDerived() = 0;
	int numEst;							
	// Number of estimates characterizing bhavioural options
	//void (agent::*pointGNO)(client newOptions[], int &idNewOptions, double &VisProbLeav, double &ResProbLeav, double &biasRV, double &negReward) = NULL;                // C++
protected:
	double featWeightsCrit[6];
	// vector of weights to fit the state-action value function. 
	// Each corresponds to one feature of the client
	double featWeightsAct[6];
	// vector of weights to fit the state-action value function. 
	// Each corresponds to one feature of the client
	double valuesT;
	// value estimated for current state
	double valuesT1;
	// value estimated for future state state
	double prefT;
	// calculated preference for options available in the current state
	double prefT1;
	// calculated preference for options available in the future state
	int choiceT;		
	// current choice 
	int choiceT1;
	// future choice
	double currentReward;				
	// reward given by current state action pair
	double cumulReward;
	// Cumulative reward
	int age;
	double negReward;
private:
	double alphaCrit;
	// speed of learning for the critic
	double alphaAct;
	// speed of learning for the actor
	double gamma;
	// importance of future rewards
	bool neta;
};

// Members of agent class

agent::agent(double alphaCI=0.01, double alphaAI=0.01, 
	double gammaI=0.5, bool netaI=0,double InitVal=0) {
	// parameterized constructor
	numEst = 6;
	client noClient = client();
	for (int i = 0; i < numEst; i++) {
		featWeightsCrit[i] = 0;
		featWeightsAct[i] = 0;
	}
	//values[4] = 10, values[2] = 10;
	alphaCrit = alphaCI, alphaAct = alphaAI, gamma = gammaI, neta = netaI;
	cleanOptionsT[0] = noClient, cleanOptionsT[1] = noClient, choiceT = 2;
	cleanOptionsT1[0] = noClient, cleanOptionsT1[1] = noClient, choiceT1 = 0;  
	valuesT = 0, valuesT1 = 0;
	prefT = 0, prefT1= 0;
	currentReward = 0, cumulReward = 0, negReward = 0;
	age = 0;
}

void agent::rebirth(double InitVal=0) {
	age = 0;
	client noClient = client();
	cleanOptionsT[0] = noClient, cleanOptionsT[1] = noClient, choiceT = 2;
	cleanOptionsT1[0] = noClient, cleanOptionsT1[1] = noClient, choiceT1 = 0;
	valuesT = 0, valuesT1 = 0;
	prefT = 0, prefT1 = 0;
	currentReward = 0;
	cumulReward = 0;
	negReward = 0;
	for (int i = 0; i < numEst; i++) {
		featWeightsCrit[i] = 0;
		featWeightsAct[i] = 0;
	}
	//values[4] = 10, values[2] = 10;
}

agent::~agent() {}		
// Destructor

void agent::checkChoice() {
	if (choiceT>1)
	{
		error("agent::act", "choice is not among the options");
	}
}

double agent::getLearnPar(learPar parameter) {
	if (parameter == alphaCritPar) { return alphaCrit; }
	else if (parameter == gammaPar) { return gamma; }
	else if (parameter == alphaActPar) { return alphaAct; }
	else { return neta; }
}

void agent::ObtainReward() {
	currentReward = cleanOptionsT[choiceT].reward;
	cumulReward += cleanOptionsT[choiceT].reward;
}

void agent::getNewOptions(client newOptions[], int &idNewOptions, 
	nlohmann::json param, rnd::discrete_distribution visitSpProb,
	rnd::discrete_distribution residSpProb) {
	if (choiceT == 0) {
		// Define the behaviour of the unattended client
		if (cleanOptionsT[1].mytype == resident) {
			if (rnd::uniform() > param["ResProbLeav"].get<double>()) {
				// if the unttended client is a resident, 
				// it leaves with probability ResPropLeave
				cleanOptionsT1[1] = cleanOptionsT[1], negReward = 0; 
			}									
			else { negReward = param["negativeRew"].get<double>(); }
		}
		else if (cleanOptionsT[1].mytype == visitor) {
			if (rnd::uniform() > param["VisProbLeav"].get<double>()) {
				// if the unttended client is a visitor, 
				// it leaves with probability VisPropLeave
				cleanOptionsT1[1] = cleanOptionsT[1], negReward = 0; 
			}								
			else { negReward = param["negativeRew"].get<double>(); }
		}
		else { negReward = 0; }
	}
	else {
		if (cleanOptionsT[0].mytype == resident) {
			if (rnd::uniform() > param["ResProbLeav"].get<double>()) {
				// if the unattended client is a resident, 
				// it leaves with probability ResPropLeave
				cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; 
			}		
			else { negReward = param["negativeRew"].get<double>(); }
		}
		else if (cleanOptionsT[0].mytype == visitor) {
			if (rnd::uniform() > param["VisProbLeav"].get<double>()) {
				// if the unattended client is a visitor, 
				// it leaves with probability VisPropLeave
				cleanOptionsT1[0] = cleanOptionsT[0], negReward = 0; 
			}		
			else { negReward = param["negativeRew"].get<double>(); }
		}
		else { negReward = 0; }
	}
	if (param["experiment"].get<bool>()) { getExperimentalOptions(param,
		visitSpProb,residSpProb); }
	else { getExternalOptions(newOptions, idNewOptions, param,
		visitSpProb,residSpProb); }
}

void agent::getExternalOptions(client newOptions[], int &idNewOptions, 
	nlohmann::json param, rnd::discrete_distribution visitSpProb,
	rnd::discrete_distribution residSpProb) {
	if (cleanOptionsT1[0].mytype + cleanOptionsT1[1].mytype > 3 ) {
		// If none of the clients stayed from the previous interaction
		bool randPos = rnd::bernoulli();
		cleanOptionsT1[randPos] = newOptions[idNewOptions]; 
		++idNewOptions;
		if (cleanOptionsT1[randPos].mytype == absence) {
			// If the first draw does not yield a client
			cleanOptionsT1[!randPos] = newOptions[idNewOptions], ++idNewOptions;
			return;
		}
	}

	if (cleanOptionsT1[0].mytype + cleanOptionsT1[1].mytype < 4) {
		// There is a client in the stations
		// Fill the alternative option depending on the available one
		bool filledPos = cleanOptionsT1[1].mytype != absence;
		double probs[3] = { (1 - static_cast<double>(param["inbr"]))*
			(1 - static_cast<double>(param["outbr"])) + 
			static_cast<double>(param["inbr"])*
			static_cast<double>(param["outbr"]) , 0, 0 };
		// Define probabilities depending on parameters
		probs[1] = probs[0] + static_cast<double>(param["inbr"])*
			(1 - static_cast<double>(param["outbr"]));
		// First prob is of a random option	
		probs[2] = probs[1] + static_cast<double>(param["outbr"])*
			(1 - static_cast<double>(param["inbr"]));
		// Second and third homophily, and heterophily respectively
		if (probs[2] != 1) {
			error("agent:getExternalOptions", 
				"probability does not sum up to 1");
		}
		double rand = rnd::uniform();
		if (probs[0] > rand) { 
			cleanOptionsT1[!filledPos] = newOptions[idNewOptions];
			++idNewOptions;
		}						
		else if (probs[1] > rand) {
			if (cleanOptionsT1[1].mytype == resident) {
				// homophily
				std::string chosenSp = "Sp";
				chosenSp.append(itos(residSpProb.sample()+1));
				cleanOptionsT1[!filledPos].rebirth(resident,
					param["residents"][chosenSp]["means"],
					param["residents"][chosenSp]["sds"],
					mins, param["residents"][chosenSp]["reward"],chosenSp);
			}
			else {
				std::string chosenSp = "Sp";
				chosenSp.append(itos(residSpProb.sample()+1));
				cleanOptionsT1[!filledPos].rebirth(visitor,
					param["visitors"][chosenSp]["means"],
					param["visitors"][chosenSp]["sds"],
					mins, param["visitors"][chosenSp]["reward"],chosenSp);
			}				
		}
		else {
			// heterophily
			if (cleanOptionsT1[0].mytype == resident) { 
				std::string chosenSp = "Sp";
				chosenSp.append(itos(visitSpProb.sample()+1));
				cleanOptionsT1[!filledPos].rebirth(visitor,
					param["visitors"][chosenSp]["means"],
					param["visitors"][chosenSp]["sds"], mins, 
					param["visitors"][chosenSp]["reward"],chosenSp);
			}
			else { 
				std::string chosenSp = "Sp";
				chosenSp.append(itos(residSpProb.sample()+1));
				cleanOptionsT1[!filledPos].rebirth(resident,
					param["residents"][chosenSp]["means"], 
					param["residents"][chosenSp]["sds"],
					mins, param["residents"][chosenSp]["reward"], chosenSp); }
		}
	}
}

void agent::getExperimentalOptions(nlohmann::json param,
	rnd::discrete_distribution visitSpProb,
	rnd::discrete_distribution residSpProb) {
	// Get new options in an experimental setting
	if (cleanOptionsT[0].mytype == resident && 
		cleanOptionsT[1].mytype == visitor) { return; }	
	// Every other option is a Resident-Visitor
	else {
		std::string chosenSp = "Sp";
		chosenSp.append(itos(residSpProb.sample()+1));
		cleanOptionsT1[0].rebirth(resident, 
			param["residents"][chosenSp]["means"],
			param["residents"][chosenSp]["sds"],mins,
			param["residents"][chosenSp]["reward"],chosenSp);
		chosenSp = "Sp";
		chosenSp.append(itos(visitSpProb.sample()+1));
		cleanOptionsT1[1].rebirth(visitor, 
			param["visitors"][chosenSp]["means"],
			param["visitors"][chosenSp]["sds"],	mins,
			param["visitors"][chosenSp]["reward"],chosenSp);
		return;
	}
}

void agent::act(client newOptions[], int &idNewOptions, 
	nlohmann::json param,
	rnd::discrete_distribution visitSpProb,	
	rnd::discrete_distribution residSpProb) {
	// taking action, obatining reward, seeing new state, choosing future action
	++age;
	// new time step
	cleanOptionsT[0] = cleanOptionsT1[0], cleanOptionsT[1] = cleanOptionsT1[1];
	// Future state becomes current state
	choiceT = choiceT1;
	// Future action becomes current action
	valuesT = valuesT1;
	prefT = prefT1;
	checkChoice();		
	// Check that the choice is among the options
	cleanOptionsT1[0].rebirth();
	cleanOptionsT1[1].rebirth();	
	// Future state is unknown: only the first parameters matters for this function. 
	choiceT1 = 2;
	ObtainReward();
	getNewOptions(newOptions, idNewOptions, param, visitSpProb, residSpProb);
	valuesT1 = value();
	prefT1 = preference();
	choice();
}
/*void agent::update(double &reward)																				// change estimated value according to current reward and estimates of future state-action pair
{
	for (size_t i = 0; i < (numEst-1)/2;i++)
	{
		if (i<8)
		{
			featWeights[i] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[0].featQuant[i];
			featWeights[i+ (numEst - 1) / 2] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[1].featQuant[i];
		}
		else
		{
			featWeights[i] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[0].featBool[i-8];
			featWeights[i + (numEst - 1) / 2] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*cleanOptionsT[1].featBool[i-8];
		}
		
	}
	featWeights[22] += alpha*(reward + gamma*valuesT1[choiceT1] - valuesT[choiceT])*choiceT;	
}*/
void agent::updateClient(int &countWeights, int clientId) {
	// change estimated value according to current reward and 
	// estimates of future state-action pair
	double delta = currentReward + negReward*neta + 
		gamma*valuesT1	- valuesT;
	double p0 = logist(prefT);
	double eligVec;
	if (choiceT) {
		eligVec = -p0;
	}
	else {
		eligVec = (1-p0);
	}
	for (int i = 0; i < cleanOptionsT[clientId].numFeat[0] + 
		cleanOptionsT[clientId].numFeat[1]; i++) {
		if (i<cleanOptionsT[clientId].numFeat[0]) {
			featWeightsCrit[countWeights] += alphaCrit*delta*
				cleanOptionsT[clientId].featQuant[i];
			featWeightsAct[countWeights] += alphaAct*delta*eligVec*
				cleanOptionsT[clientId].featQuant[i];
			++countWeights;
		}
		/*else {
			featWeightsCrit[countWeights] += 
				boolExtUpdate*alphaCrit*delta*
				cleanOptionsT[clientId].featBool[i - cleanOptionsT[clientId].numFeat[0]];
			featWeightsAct[countWeights] +=
				boolExtUpdate*alphaAct*delta*eligVec*
				cleanOptionsT[clientId].featBool[i - cleanOptionsT[clientId].numFeat[0]];
			++countWeights;
		}*/
	}
}

void agent::printIndData(std::ofstream &learnSeries, int &seed) {
	learnSeries << seed << '\t' << age << '\t';
	learnSeries << alphaCrit << '\t' << alphaCrit << '\t' << gamma << '\t';
	learnSeries << neta << '\t';
	learnSeries << currentReward << '\t' << cumulReward << '\t' << negReward << '\t';
	learnSeries << valuesT << '\t' << prefT << '\t' << choiceT << '\t';
	cleanOptionsT[choiceT].printClientData(learnSeries);
	if (choiceT == 0) {	
		cleanOptionsT[1].printClientData(learnSeries);	
	}
	else { 
		cleanOptionsT[0].printClientData(learnSeries); 
	}
	for (int j = 0; j < numEst; j++) {
		learnSeries << featWeightsCrit[j] << '\t';
		//cout << values[j] << '\t';
	}
	for (int j = 0; j < numEst; j++) {
		learnSeries << featWeightsAct[j] << '\t';
		//cout << values[j] << '\t';
	}
	learnSeries << std::endl;
	//cout << endl;
}


double agent::logist(double diffPref) { 
	return (1 / (1 + exp(-diffPref))); 
}

double agent::linearComClient(int clientid, int &countFeat, 
	double featWeights[]) {
	double temp = 0;
	for (int i = 0; i < cleanOptionsT1[clientid].numFeat[0] +
		cleanOptionsT1[clientid].numFeat[1]; i++) {
		if (i < cleanOptionsT1[clientid].numFeat[0]) {
			temp += cleanOptionsT1[clientid].featQuant[i] *
				featWeights[countFeat];
			++countFeat;
		}
		/*else {
			temp += cleanOptionsT1[clientid].featBool[i - cleanOptionsT1[clientid].numFeat[0]]
				* featWeights[countFeat];
			++countFeat;
		}*/
	}
	return(temp);
}

class FIATy1																								// Agent that estimates state-action and uses value for every desicion
	:public agent {
public:
	FIATy1(double alphaCritI, double alphaActI, double gammaI, bool netaI)
		:agent(alphaCritI, alphaActI, gammaI, netaI)	{
			numEst = 6;
		}
	virtual void choice() {
		if (rnd::uniform() < logist(prefT1)) { choiceT1= 0; }
		else { choiceT1 = 1; }
	}
	virtual void updateDerived() {
		int countWeights = 0;
		updateClient(countWeights, 0);
		updateClient(countWeights, 1);
	}
	virtual double value() {
		int countFeat = 0;
		double client0 = 0;
		double client1 = 0;
		client0 = linearComClient(0, countFeat,featWeightsCrit);
		client1 = linearComClient(1, countFeat,featWeightsCrit);
		return(client0 + client1);
	}
	virtual double preference() {
		int countFeat = 0;
		double client0 = 0;
		double client1 = 0;
		client0 = linearComClient(0, countFeat, featWeightsAct);
		client1 = linearComClient(1, countFeat, featWeightsAct);
		return(client0 + client1);
	}
};



class FIATy2 :public agent {
	// Agents that estimates state-actions and only uses value to descriminate between clients
public:
	FIATy2(double alphaCritI, double alphaActI, double gammaI, bool netaI)
		:agent(alphaCritI, alphaActI, gammaI, netaI) {
		numEst = 6;
	}
	virtual void choice() {
		if (cleanOptionsT1[0].mytype != absence &&
			cleanOptionsT1[1].mytype != absence) {
			// if there are no absences, then use desicion rule
			if (rnd::uniform() < logist(prefT1)) {
				choiceT1 = 0;
			}
			else { choiceT1 = 1; }
		}
		else if (cleanOptionsT1[0].mytype == absence) {
			// if there is an absence, then chose the other option
			choiceT1 = 1;
		}
		else {
			choiceT1 = 0;
		}
	}
	virtual void updateDerived() {
		int countWeights = 0;
		updateClient(countWeights, 0);
		updateClient(countWeights, 1);
	}
	virtual double value() {
		int countFeat = 0;
		double client0 = 0;
		double client1 = 0;
		client0 = linearComClient(0, countFeat, featWeightsCrit);
		client1 = linearComClient(1, countFeat, featWeightsCrit);
		return(client0 + client1);
	}
	virtual double preference() {
		int countFeat = 0;
		double client0 = 0;
		double client1 = 0;
		client0 = linearComClient(0, countFeat, featWeightsAct);
		client1 = linearComClient(1, countFeat, featWeightsAct);
		return(client0 + client1);
	}
};



//class PIATy1 :public agent {
//	// Agents that estimates actions and uses value for every desicion
//public:
//	PIATy1(double alphaI, double gammaI, double tauI, double netaI)
//		:agent(alphaI, gammaI, tauI, netaI)	{
//		numEst = 11;
//	}
//	virtual void choice(int &StaAct1, int &StaAct2)	{
//		if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))	{
//			choiceT1 = 0;
//		}
//		else { choiceT1 = 1; }
//	}
//	virtual double value() {
//		int countFeat = 0;
//		return(valueClient(choiceT1,countFeat));
//	}
//	virtual void updateDerived() {
//		int countWeights = 0;
//		updateClient(countWeights, choiceT);
//	}
//};
//
//class PIATy2 :public agent {
//	// Agents that estimates actions and only uses value to descriminate between clients
//public:
//	PIATy2(double alphaI, double gammaI, double tauI, double netaI)
//		:agent(alphaI, gammaI, tauI,netaI) {
//		numEst = 11;
//	}
//	virtual void choice(int &StaAct1, int &StaAct2)	{
//		if (cleanOptionsT1[0].mytype != absence && 
//			cleanOptionsT1[1].mytype != absence) {
//			// if there are no absences, then use desicion rule
//			if (rnd::uniform() < softMax(valuesT1[0], valuesT1[1]))	{
//				choiceT1 = 0;
//			}
//			else { choiceT1 = 1; }
//		}
//		else if (cleanOptionsT1[0].mytype == absence) {
//			// if there is an absence, then chose the other option
//			choiceT1 = 1;
//		}
//		else {
//			choiceT1 = 0;
//		}
//	}
//	virtual void updateDerived() {
//		int countWeights = 0;
//		updateClient(countWeights, choiceT);
//	}
//	virtual double value()	{
//		int countFeat = 0;
//		return(valueClient(choiceT1,countFeat));
//	}
//};


#endif //!CLEANER_H