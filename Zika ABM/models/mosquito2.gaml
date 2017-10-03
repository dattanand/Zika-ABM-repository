/**
 *  model4
 *  This model illustrates how to use spatial operator
 */ 
model SI_city

global{ 
	int nb_people <- 550;
	int nb_infected_init <- 1;	
	int nb_water_sources <- 5 update: water_source count(each.current_age>=0);
	int nb_eggs <- 0 update: egg count(each.age>=0);
	
	file roads_shapefile <- file("../includes/roads.shp");
	file buildings_shapefile <- file("../includes/buildings.shp");
	geometry shape <- envelope(roads_shapefile);
	int nb_people_infected <- nb_infected_init update: people count (each.is_infected);
	int nb_people_not_infected <- nb_people - nb_infected_init update: nb_people - nb_people_infected;
	float infected_rate update: nb_people_infected/nb_people;
	int nb_mosquito_infected <- 0 update: mosquito count (each.is_infected);
	int nb_egg_infected <- 0 update: egg count (each.is_infected);
	int nb_infected_cumulative <- nb_infected_init;
	float sensor_range <- 3.0 #m;
    int mosquito_no <- 1000;
	
	
	float step <- 10 #mn;
	
	float fetal_mortality <- 0.01;
	float mortality_emergence <- 0.02;
	float prob_mating <- 0.2;
	float sex_ratio <- 0.5;	
	
	float mortality_rate <- 0.05; 
	int max_meals <- 2; 
	float prob_reproduce <- 0.01;
	float adult_mortality <- 0.05;
	
	
	
	
	
	int current_hour update: (time/3600) mod 24;
	int days_passed update: int(time/86400);
	int current_month update: int(days_passed/30);
	bool is_night <- true update: current_hour < 7 or current_hour > 20;	
	
	

	int nb_protected <- 0;
	
	/* City Details */
	building school;
	building office;
	

	graph road_network;
	
	init{
		
		
		create road from: roads_shapefile;
		road_network <- as_edge_graph(road);
		create building from: buildings_shapefile;		
		create lake number:1;
		create people number:nb_people {
			my_house <- one_of(building);
			location <- any_location_in(my_house);			
			state_duration[1] <- 2+rnd(4); // 2-6 days
			state_duration[2] <- 4+rnd(3); // 4-7 days
			state_duration[3] <- 14+rnd(90); // 2 weeks to 3 months
		}
		
		create water_source number:nb_water_sources {
			create egg number:25{
				my_water_source <- myself;
				is_infected <- false;
				max_age <- 8;
				age <- 0;
			}
		}
		
		create mosquito  number:mosquito_no {
		
				my_water_source <- one_of(water_source);
				location <- any_location_in(my_water_source);	
				mos_age <- 0;			
			
		}
		
		ask nb_infected_init among people {
			is_infected <- true;
			state <- 1;
		}
		
		ask nb_protected among people {
			is_protected <- true;
		}
		office <- one_of(building);
		
		
		
		
	}
	
reflex rain when: (time mod 86400 = 0) {
		
		if flip(/*rain_probabilty*/0.5) {
			create water_source number:int(5);
		}
	} 
	
	reflex end_simulation when: infected_rate = 1.0 or (nb_people_infected=0 and nb_mosquito_infected=0 and nb_egg_infected=0){
		do pause;
	}
}


species egg {
	int age <- 0;
	bool is_infected <- false;
	int max_age;
	water_source my_water_source;
	reflex age when: time mod 86400=0{
		age <- age + 1;
		if flip(1 - fetal_mortality){
			if(flip(1 - mortality_emergence)){				
			
		if(age>=max_age){
			if flip(sex_ratio){
				create mosquito {
					my_water_source <- myself.my_water_source;
					location <- any_location_in(my_water_source);
					type <- rnd(1);
					is_infected <- is_infected;
						mos_age <- 0;
				}
				mosquito_no <- mosquito_no+1;		
				do die;
			}
			
			
		}
		}
			
		}
	}
}
species mosquito skills:[moving]{	
	float speed <- (0.1 + rnd(1.0)) #km/#h;
	bool is_infected <- false;
	water_source my_water_source;
	int type <- 0; // 0->A.Aegypti 1->A.Albopictus		
	int time_since_virus <- 0;
	int time_to_mature <- 3 ;	
	
	int num_meals_today <- 0;
	bool carrying_eggs <- false;
	int time_since_eggs <- 0;
	int mos_age <- 0;
	int adult_lifespan <- 5;
	int time_passed_virus <- 0;
	
	reflex move when:  !(current_hour < 7 or current_hour > 20){
		do wander amplitude:350 #m;
	}

	reflex age when: time mod 86400=0{
		mos_age <- mos_age + 1;
		if(mos_age > adult_lifespan){				
			do die;
			mosquito_no <- mosquito_no-1;
		}
	}
	
	reflex feed when:  current_hour>=9 and current_hour<=18 and time mod 600 = 0 and num_meals_today<max_meals{
		if is_infected{
			ask any (people at_distance sensor_range) {
				if !(is_protected and in_my_house){
					myself.num_meals_today <- myself.num_meals_today + 1;
					if myself.is_infected{
						float p_trans <- 0.6;
						if (state=0){
							if flip(p_trans){	
								is_infected <- true;
								state <- 1;
								nb_infected_cumulative <- nb_infected_cumulative+1;
							}
						}
					}
				}
			}
		}
		else {
			ask any (people at_distance sensor_range) {
				if !(is_protected and in_my_house) {
					myself.num_meals_today <- myself.num_meals_today + 1;
					if is_infected{
						float p_trans <- 0.5;
						if flip(p_trans) {
							myself.is_infected <- true;
						}
					}
				}
			}
		}
	}
	
	
	reflex reproduce when: time_since_eggs>=3 and time mod 600 = 0{
			ask water_source at_distance 20 #m {
			bool infection <-  myself.is_infected;
			create egg number:10{
				my_water_source <- myself;
				is_infected <- infection;
				age <- 0;
				max_age <- 8;
			}		
		
		
		}
		time_since_eggs <- 0;
		carrying_eggs <- false;
	}
	reflex nextDay when: time mod 86400 = 0{
		
		if flip(adult_mortality){
			mosquito_no <- mosquito_no-1;
			do die;
		}
		num_meals_today <- 0;
		if(carrying_eggs){
			time_since_eggs <- time_since_eggs+1;
		}		
		if(is_infected){
			time_passed_virus <- time_passed_virus;
		}
		if(!carrying_eggs){
			if flip(prob_mating){
				if flip(sex_ratio){
				carrying_eggs <- true;
				}
			}
		}
	
	
}
aspect triangle{
		draw triangle(10) color:is_infected ? #red : #green;
	}
}

species people skills:[moving]{		
	float speed <- (2 + rnd(3)) #km/#h;
	bool is_infected <- false;
	building my_house;
	point target;
	bool in_my_house <- true;
	int state <- 0; // 0->Susceptible 1->Exposed 2->Infected 3-> Cured
	list<int> state_duration <- [0,0,0,0]; 
	int days_infected <- 0;	
	bool is_protected <- false;	
		
	reflex move when: target != nil and !is_night and (state=0 or state=1 or state=3){
		do goto target:target on: road_network;
		if (location = target) {
			target <- nil;
		} 
	}
	
	reflex health when: (state=1 or state=2) and (time mod 86400 = 0){		
			days_infected <- days_infected+1;
		if state = 1 and (days_infected - state_duration[1] = 0){
				state <- 2;
		}
		else if state = 2 and (days_infected - state_duration[1] - state_duration[2] = 0){				
					state <- 3;				
				is_infected <- false;
		}	
	}
	
	reflex set_target when: target=nil {
		
				if flip(0.7){
					target <- any_location_in (my_house);
						in_my_house <- true;
					}				
					else {
						building bd_target <- one_of(building);
				target <- any_location_in (bd_target);
						in_my_house <- false;
					}
	}
	aspect circle{
		draw circle(10) color:is_infected ? #red : #green;
	}
}

species road {
	aspect geom {
		draw shape color: #black;
	}
}

species building {	
	aspect geom {
		draw shape color: #gray;
	}
}



species lake{
	aspect square{
		draw square(150) at: {75,1700} color: #blue;
	}
}

species water_source {
	int current_age <- 0; 
	aspect square{
		draw square(20) color: #blue;
	}
	reflex age when: (time mod 86400 = 0) {
		current_age <- current_age + 1;
	}
	reflex vanish when: current_age = 8 {
		do die;
	}
}

experiment main_experiment type:gui{
	
	output {
			monitor "Infected people rate" value: infected_rate;
		monitor "Current Day" value: days_passed;
		monitor "Current Hour" value: current_hour;
		monitor "Current Month" value: current_month;
		monitor "Current Time" value: time/60;
		monitor "Current water sources" value: nb_water_sources;
		monitor "Infected people" value: nb_people_infected; 
		monitor "Infected mosqutios" value: nb_mosquito_infected;
		monitor "Infected eggs" value: nb_egg_infected; 
		
display chart1 refresh_every: 10 {
			chart "Disease spreading" type: series {
//				data "susceptible" value: nb_people_not_infected color: #green;
				data "infected" value: nb_people_infected color: #red;
				data "infected cumulative (/500)" value: nb_infected_cumulative color: #blue;
			}
		}
		
		display chart2 refresh_every: 10 {
			chart "Mosquito Population" type: series {
				data "Mosquito_Population" value: mosquito_no color: #blue;
				data "infected" value: nb_mosquito_infected color: #red;
				data "num_eggs" value: nb_eggs color: #black;
			}
		}
		
	}
}


