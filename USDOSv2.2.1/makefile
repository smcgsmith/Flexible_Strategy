CC        =g++
CFLAGS    =-c -std=gnu++11 -Wall -O3 
LDFLAGS   =-lstdc++ -L/usr/lib/x86_64-linux-gnu -lgsl -lgslcblas -lm
INCLUDE   =-I./include/ 
OBJDIR    =obj/

# Backslash for linebreak. Wohoo!! Yay! (and comment is # obv. Beware, it affects the *whole* line.) This makefile is for J!!!
OBJLIST   = Point.o Farm.o Shipment_kernel.o \
		 	Region.o County.o State.o Grid_cell.o Grid_checker.o \
			Grid_manager.o Status_manager.o	shared_functions.o \
			File_manager.o main.o Control_manager.o Shipment_manager.o \
			Local_spread.o Control_resource.o USAMMv2_parameters.o \
			USAMMv3_parameters.o Diagnostic_manager.o Diagnostic_resource.o

OBJECTS   = $(addprefix $(OBJDIR), $(OBJLIST) )

all:USDOSv2_new

USDOSv2_new: $(OBJECTS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

$(OBJECTS): ./$(OBJDIR)%.o: src/%.cpp
	$(CC) $(CFLAGS) $? -o $@ $(INCLUDE)

clean:
	rm obj/*.o
