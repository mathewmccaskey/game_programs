#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

using namespace std;

//------------------------------------------------------------------------------------------------//
class board
//------------------------------------------------------------------------------------------------//
//                                                                                                //
//  This class gives all the necessary parameters and functions used for dealing with a board     //
//  for connect 4.                                                                                //
//                                                                                                //
//------------------------------------------------------------------------------------------------//
{
  public: 
    bool winning_move[2][7], ending_board;
    int moves_per_col[7], next_board[7], winner;

    // Public functions and subroutines
    void read_board_state(ifstream&);
    void write_board_state(ofstream&);
    void print_board();
    void add_move(int, int);
    void find_winning_moves();
    bool operator==(const board&);
        
  private:
    int board_state[7][6];
};



//------------------------------------------------------------------------------------------------//
void board::read_board_state(ifstream& input_board)
//------------------------------------------------------------------------------------------------//
//                                                                                                //
//  This function reads in a board state from an open file given via the ifstream variable.       //
//                                                                                                //
//------------------------------------------------------------------------------------------------//
{
  // Variables used in this subroutine only
  int number;
  
  // Read in a dummy number
  input_board >> number;
  
  // Read in the board state
  for (int x1=5; x1>=0; x1--)
    for (int x2=0; x2<7; x2++)
      input_board >> this->board_state[x2][x1];
}



//------------------------------------------------------------------------------------------------//
void board::write_board_state(ofstream& output_board)
//------------------------------------------------------------------------------------------------//
//                                                                                                //
//  This function writes a board state to an open file given via the ofstream variable.           //
//                                                                                                //
//------------------------------------------------------------------------------------------------//
{
  // Write out the board state
  for (int x1=5; x1>=0; x1--){
    for (int x2=0; x2<7; x2++)
      output_board << "  " << this->board_state[x2][x1];
    output_board << endl;
  }
}



//------------------------------------------------------------------------------------------------//
void board::print_board()
//------------------------------------------------------------------------------------------------//
//                                                                                                //
//  This function is mainly for testing purposes.  It prints out all the information on a given   //
//  game board.                                                                                   //
//                                                                                                //
//------------------------------------------------------------------------------------------------//
{  
  // Variables used in this subroutine only
  cout << "Ending board: " << this->ending_board << endl;
  
  cout << "\nNumber of moves per column: " << endl;
  for (int x1=0; x1<7; x1++){
    cout << this->moves_per_col[x1] << "  ";
  }
  
  cout << "\nNext boards: " << endl;
  for (int x1=0; x1<7; x1++){
    cout << this->next_board[x1] << "  ";
  }

  cout << "\nWinning move for player 1: " << endl;
  for (int x1=0; x1<7; x1++){
    cout << this->winning_move[0][x1] << "  ";
  }
  cout << "\nWinning move for player 2: " << endl;
  for (int x1=0; x1<7; x1++){
    cout << this->winning_move[1][x1] << "  ";
  }

  cout << "\nBoard State: " << endl;
  for (int x1=5; x1>=0; x1--){
    for (int x2=0; x2<7; x2++)
      cout << this->board_state[x2][x1] << "  ";
    cout << endl;
  }
}



//------------------------------------------------------------------------------------------------//
void board::add_move(int move, int player)
//------------------------------------------------------------------------------------------------//
//                                                                                                //
//  This subroutine will take the input move and add it to the list of moves as well as add it    //
//  to the board state.  After it will check to see if any potential moves are blocked off        //
//  i.e. one column is completely full.                                                           //
//                                                                                                //
//  Input: move - the column of the board that the move is going to go.                           //
//         player - this shows which player's turn it is.  This goes into the whole board state.  //
//         check_winning_moves - Boolean that allows one to check if there are winning moves      //
//                               after the move has been made.                                    //
//                                                                                                //  
//------------------------------------------------------------------------------------------------//
{    
  // Add the move to the complete board state
  this->board_state[move][moves_per_col[move]] = player;
  this->moves_per_col[move]++;
}



//------------------------------------------------------------------------------------------------//
void board::find_winning_moves()
//------------------------------------------------------------------------------------------------//
//                                                                                                //
//  This subroutine will take a board state and test it to see if there are winning moves for     //
//  either player.  That way when we generate boards, we only concern ourselves whith boards that //
//  minimize the number of moves (i.e. if a player first can win they do, else if they can block  //
//  the other player they do.)                                                                    //
//                                                                                                //
//------------------------------------------------------------------------------------------------//
{
  // Parameters used in this routine only
  int dummy_board[7][6], player, column, goal;
  
  // Initiailize the winning moves matrix for the board
  for (int x1=0; x1<2; x1++)
    for (int x2=0; x2<7; x2++)
      this->winning_move[x1][x2] = false;
  
  // Copy the board to the dummy board
  for (int x1=0; x1<7; x1++)
    for (int x2=0; x2<6; x2++)
      dummy_board[x1][x2] = this->board_state[x1][x2];
  
  // Loop over both players
  for (player=1; player<=2; player++){
    
    // The goal for getting four in a row is the product of four consecutive cells to be the player number^4
    goal = player*player*player*player;
  
    // Loop over the columns where the potential winning move can be made
    for (column=0; column<7; column++){
    
      // The column first needs to have an available move
      if (this->moves_per_col[column] < 5){
    
        // Add the test move to the dummy board
        dummy_board[column][this->moves_per_col[column]] = player;
        // Check horizontal 4 in a row
        for (int x1=0; x1<6; x1++)
          for (int x2=0; x2<4; x2++)
            if (dummy_board[x2][x1]*dummy_board[x2+1][x1]*dummy_board[x2+2][x1]*dummy_board[x2+3][x1] == goal)
              this->winning_move[player-1][column] = true;
        
        // Check vertical 4 in a row
        if (!this->winning_move[player-1][column]){
          for (int x1=0; x1<7; x1++)
            for (int x2=0; x2<3; x2++)
              if (dummy_board[x1][x2]*dummy_board[x1][x2+1]*dummy_board[x1][x2+2]*dummy_board[x1][x2+3] == goal)
                this->winning_move[player-1][column] = true;
        }
        
        // Check upward diagonal 4 in a row
        if (!this->winning_move[player-1][column]){
          for (int x1=0; x1<4; x1++)
            for (int x2=0; x2<3; x2++)
              if (dummy_board[x1][x2]*dummy_board[x1+1][x2+1]*dummy_board[x1+2][x2+2]*dummy_board[x1+3][x2+3] == goal)
                this->winning_move[player-1][column] = true;
        }
        
        // Check downward diagonal 4 in a row
        if (!this->winning_move[player-1][column]){
          for (int x1=0; x1<4; x1++)
            for (int x2=3; x2<6; x2++)
              if (dummy_board[x1][x2]*dummy_board[x1+1][x2-1]*dummy_board[x1+2][x2-2]*dummy_board[x1+3][x2-3] == goal)
                this->winning_move[player-1][column] = true;
        }
        
        // Reset the test move we just made so we don't have to reset the whole game board
        dummy_board[column][this->moves_per_col[column]] = 0;
      }
    }
  }
}



//------------------------------------------------------------------------------------------------//
bool board::operator==(const board& comp_board)
//------------------------------------------------------------------------------------------------//
//                                                                                                //
//                                                                                                //
//------------------------------------------------------------------------------------------------//
{
  // Variables used in this function only
  bool check1, check2;
  int box_index, column, row;
  
  // Initialize the checks
  check1 = true;
  check2 = true;
  
  // Compare the boards directly (both forwards and backwards)
  box_index = 0;
  while(box_index < 42 && (check1 || check2)){
    // Get the coordinates from the box index
    column = box_index%7;
    row = box_index/7;
      
    // Compare the board state with the comp_board and the reverse of the comp_board
    if (this->board_state[column][row] != comp_board.board_state[column][row])
      check1 = false;
    if (this->board_state[6-column][row] != comp_board.board_state[column][row])
      check2 = false;
    
    box_index++;
  }
  
  // If either one of these checks are true then the board is equivalent to comp_board (i.e. it isn't unique)
  if (check1 || check2)
    return true;
  else
    return false;
}



//------------------------------------------------------------------------------------------------//
int main()
//------------------------------------------------------------------------------------------------//
//                                                                                                //
//  This program is an attempt to solve the game connect 4.                                       //
//                                                                                                //  
//  Using a class that describes an individual board state this will first generate all the       //
//  necessary game board with some slight modifications to make the problem tractable.            //
//                                                                                                //  
//  While generating game boards if there is any board  where a single move will win the game     //
//  or a move that can block a potential four in a row will be taken.  There really is no point   //
//  generating every single possible unique board if it is possible to win in a fewer number of   //
//  moves.                                                                                        //
//                                                                                                //
//  After the boards are generated we will run a neural network to find the optimal strategy to   //
//  win the game.  All the boards as well as the next possible boards and the optimal solution    //
//  will be output to some text files.                                                            //
//                                                                                                //
//------------------------------------------------------------------------------------------------//
{
  // Variables used in this program only
  int n_boards[43], n_ending_boards[43];
  int move, board_index, column_move, next_board_index, player, counter, board_number;
  vector <board> boards_per_move[43];
  board dummy_board;
  bool unique_board, poss_winning_move, poss_blocking_move;

  // input and output variables
  string input_file, input_info_file, input_winner_file;
  string output_move_file, output_board_file, output_board_info_file, output_winner_file;
  string output_string;
  stringstream ss;
  ifstream board_input, board_info_input, winner_input;
  ofstream board_output, board_info_output, winner_output, move_output;
  
  // initialize the number of boards
  for (int x1=0; x1<43; x1++){
    n_boards[x1] = 0;
    n_ending_boards[x1] = 0;
  }
  
  // Loop over the number of moves to generate the sequence of boards
  for (move=8; move<10; move++){
    
    // get the player number from the move
    if (move%2 == 0)
      player = 1;
    else
      player = 2;
    
    // This is just a counter to show the progress of the board generation
    counter = 0;
    
    // Input files
    // string stream to get the right files for each move
    ss.str("");
    ss.clear();
    ss << move;
    
    // input file strings
    input_file = "output/boards_" + ss.str() + ".txt";
    input_info_file = "output/board_info_" + ss.str() + ".txt";
    input_winner_file = "output/winner_" + ss.str() + ".txt";
    
    // open the input files
    board_input.open(input_file.c_str());
    board_info_input.open(input_info_file.c_str());
    winner_input.open(input_winner_file.c_str());

    // check to see if the files were successfully opened
    if (board_input.fail()){
      cout << "Board input file failed" << endl;
      return 1;
    }
    if (board_info_input.fail()){
      cout << "Board info input file failed" << endl;
      return 1;
    }
    if (winner_input.fail()){
      cout << "Winner input file failed" << endl;
      return 1;
    }
        
    // Read in the number of boards and number of ending boards for this move
    winner_input >> n_boards[move] >> n_ending_boards[move];
    cout << move << "  " << n_boards[move] << "  " << n_ending_boards[move] << endl;
    
    // loop over the number of boards in the current move number
    for (board_index=0; board_index<n_boards[move]; board_index++){
      
      // Read in the board information
      winner_input >> board_number >> dummy_board.ending_board >> dummy_board.winner;
      board_info_input >> board_number;
    
      // Read in the number of moves per column for the board
      for (int x1=0; x1<7; x1++)
        board_info_input >> dummy_board.moves_per_col[x1];
      
      // Read in the winning move information
      for (int x1=0; x1<2; x1++)
        for (int x2=0; x2<7; x2++)
          board_info_input >> dummy_board.winning_move[x1][x2];
      
      // Initialize the next moves for the dummy board
      for (int x1=0; x1<7; x1++)
        dummy_board.next_board[x1] = -1;
      
      // Read in the board state
      dummy_board.read_board_state(board_input);
      
      // Push the newly read in board onto the vector of boards for this move
      boards_per_move[move].push_back(dummy_board);
      
      
      // Keep a counter going so that we can track the progress of the board generation
      counter++;
      if (counter%1000 == 0)
        cout << counter << endl;
      
      
      // Check to make sure that the board is not a game ending board
      if (!boards_per_move[move][board_index].ending_board){
        
        // See if there is a potential game winning move for the player
        poss_winning_move = false;
        for (int x1=0; x1<7; x1++)
          if (boards_per_move[move][board_index].winning_move[player-1][x1])
            poss_winning_move = true;
        
        // Also see if there is a potential blocking move for the other player
        poss_blocking_move = false;
        for (int x1=0; x1<7; x1++)
          if (boards_per_move[move][board_index].winning_move[2-player][x1])
            poss_blocking_move = true;
        
        // Now that we have these booleans we will employ the following strategy.
        // 1. If there is a winning move for the player, that move is taken and all others are ignored
        // 2. If there are no winning moves, any potential blocking moves are taken and others are ignored
        // 3. Otherwise just analyze all the possible moves
        
        // Winning move
        if (poss_winning_move){
          
          // loop over the columns so we can find the winning moves
          for (column_move=0; column_move<7; column_move++){
            
            // If the move is a winning move then try to add it to the list of boards
            if (boards_per_move[move][board_index].winning_move[player-1][column_move]){
              dummy_board = boards_per_move[move][board_index];
              dummy_board.add_move(column_move,player);
              
              // loop over all the boards in the next move to compare with the new board
              unique_board = true;
              next_board_index = 0;
              while ((next_board_index<n_boards[move+1]) && unique_board){
                if (dummy_board == boards_per_move[move+1][next_board_index]){
                  unique_board = false;
                  // Set the next board to the non-unique board
                  boards_per_move[move][board_index].next_board[column_move] = next_board_index;
                }
                next_board_index++;
              }
          
              // If it survived the gauntlet then it is a unique board and we tack it on the board list
              // and since it is a winning board we set the appropriate variables
              if (unique_board){
                
                // Since we have a winning board then we don't care about the possible winning moves
                for (int x1=0; x1<2; x1++)
                  for (int x2=0; x2<7; x2++)
                    dummy_board.winning_move[x1][x2] = false;
                
                // add the unique winning board and set the appropriate winning variables
                boards_per_move[move+1].push_back(dummy_board);
                boards_per_move[move+1][n_boards[move+1]].ending_board = true;
                boards_per_move[move+1][n_boards[move+1]].winner = player;
                
                // Set the next board variable with the new board
                boards_per_move[move][board_index].next_board[column_move] = n_boards[move+1];
                  
                // Increment the number of boards and the number of ending boards
                n_boards[move+1]++;
                n_ending_boards[move+1]++;
              }
            }
          }
        }
        
        // Blocking move
        else if (poss_blocking_move){
          
          // loop over the columns so we can find the blocking moves
          for (column_move=0; column_move<7; column_move++){
            
            // If the move is a winning move then try to add it to the list of boards
            if (boards_per_move[move][board_index].winning_move[2-player][column_move]){
              dummy_board = boards_per_move[move][board_index];
              dummy_board.add_move(column_move,player);
              
              // loop over all the boards to compare to the new board
              unique_board = true;
              next_board_index = 0;
              while ((next_board_index<n_boards[move+1]) && unique_board){
                if (dummy_board == boards_per_move[move+1][next_board_index]){
                  unique_board = false;
                  // Set the next board to the non-unique board
                  boards_per_move[move][board_index].next_board[column_move] = next_board_index;
                }
                next_board_index++;
              }
          
              // If it survived the gauntlet then it is a unique board and we tack it on the board list
              if (unique_board){
                
                // Find the potential winning moves for the new board if player 1 has made at least 3 moves
                if (move >= 4)
                  dummy_board.find_winning_moves();
                
                boards_per_move[move+1].push_back(dummy_board);
                // If this is the 42nd move then this is a game ending board
                if (move == 41){
                  boards_per_move[move+1][n_boards[move+1]].ending_board = true;
                  n_ending_boards[move+1]++;
                }
                
                // Set the next board variable with the new board
                boards_per_move[move][board_index].next_board[column_move] = n_boards[move+1];
                
                // Increment the number of boards for this move
                n_boards[move+1]++;
              }
            }
          }
        }
        
        // Analyze all possible moves
        else{
          // loop over the number of columns to find the new possible boards
          for (column_move=0; column_move<7; column_move++){
        
            // If the move is possible (i.e. the column isn't full) then we test the uniqueness of the new board
            if (boards_per_move[move][board_index].moves_per_col[column_move] < 6){
              dummy_board = boards_per_move[move][board_index];
              dummy_board.add_move(column_move,player);
                    
              // loop over all the boards to compare to the new boards
              unique_board = true;
              next_board_index = 0;
              while ((next_board_index<n_boards[move+1]) && unique_board){
                if (dummy_board == boards_per_move[move+1][next_board_index]){
                  unique_board = false;
                  // Set the next board to the non-unique board
                  boards_per_move[move][board_index].next_board[column_move] = next_board_index;
                }
                next_board_index++;
              }
          
              // If it survived the gauntlet then it is a unique board and we tack it on the board list
              if (unique_board){
                
                // Find the potential winning moves for the new board if player 1 has made at least 3 moves
                if (move >= 4)
                  dummy_board.find_winning_moves();
                
                boards_per_move[move+1].push_back(dummy_board);
                // If this is the 42nd move then this is a game ending board with no winner
                if (move == 41){
                  boards_per_move[move+1][n_boards[move+1]].ending_board = true;
                  n_ending_boards[move+1]++;
                }
                
                // Set the next board variable with the new board
                boards_per_move[move][board_index].next_board[column_move] = n_boards[move+1];
                
                // Increment the number of boards for this move
                n_boards[move+1]++;
              }
            }
          }
        }
      }
    }
    
    // close the input files
    board_input.close();
    board_info_input.close();
    winner_input.close();
    
    // Output the list of next boards for each board in the current move
    output_move_file = "output/moves_" + ss.str() + ".txt";
    move_output.open(output_move_file.c_str());
    //move_output.open("output/moves_0.txt");
    for (int board_index=0; board_index<n_boards[move]; board_index++){
      move_output << board_index << "     ";
      for (int x2=0; x2<7; x2++)
        move_output << boards_per_move[move][board_index].next_board[x2] << "  ";
      move_output << "\n";
    }
    move_output.close();
    
    // Output the board states for the next move
    ss.str("");
    ss.clear();
    ss << (move+1);
    output_board_file = "output/boards_" + ss.str() + ".txt";
    board_output.open(output_board_file.c_str());
    
    // loop over the boards and output them to the file
    for (board_index=0; board_index<n_boards[move+1]; board_index++){
      board_output << board_index << endl;
      boards_per_move[move+1][board_index].write_board_state(board_output);
      board_output << endl;
    }
    board_output.close();
    
    // Output the board information for the next move
    output_board_info_file = "output/board_info_" + ss.str() + ".txt";
    board_info_output.open(output_board_info_file.c_str());

    // loop over the boards and output then moves_per_column and the possible winning moves
    for (board_index=0; board_index<n_boards[move+1]; board_index++){
      board_info_output << board_index << endl;
      for (int x2=0; x2<7; x2++)
        board_info_output << "  " << boards_per_move[move+1][board_index].moves_per_col[x2];
      board_info_output << "\n";
      
      for (int x2=0; x2<2; x2++){
        for (int x3=0; x3<7; x3++)
          board_info_output << "  " << boards_per_move[move+1][board_index].winning_move[x2][x3];
        board_info_output << endl;
      }
      board_info_output << endl;
    }
    board_info_output.close();

    // Output the winner information for the boards in the next move
    output_winner_file = "output/winner_" + ss.str() + ".txt";
    winner_output.open(output_winner_file.c_str());
    
    // first we output the number of boards for this move
    winner_output << n_boards[move+1] << "  " << n_ending_boards[move+1] << endl;
    winner_output << endl;
    for (board_index=0; board_index<n_boards[move+1]; board_index++){
      winner_output << "  " << board_index;
      winner_output << "  " << boards_per_move[move+1][board_index].ending_board;
      winner_output << "  " << boards_per_move[move+1][board_index].winner << endl;
    }
    winner_output.close();
  }
}