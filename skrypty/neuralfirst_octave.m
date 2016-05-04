#OCTAVE sieć neuronowa
#https://en.wikipedia.org/wiki/Backpropagation
#https://aimatters.wordpress.com/2015/12/19/a-simple-neural-network-in-octave-part-1/
#funkcja progowa sigmoidalna
function [result] = sigmoid(x)
    result = 1.0 ./ (1.0 + exp(-x));
end

#ustawiamy początek pseudolosowego generatora, aby zawsze mieć te same wyniki
rand ("seed", 1235)

A1 = [1; 0; 0];
# generacja losowej macierzy 2 na 3
# w tym przypadku wszystkich połaczeń między wejściem X1 i X2 oraz 
# 3 neuronami warstwy środkowej
THETA1 = 2*rand(2,3) - 1;
# w tym przypadku wszystkich połaczeń między wyjściem z
# sieci neuronowej oraz neuronem na wyjściu
THETA2 = 2*rand(1,3) - 1;
Z2 = THETA1 * A1;
A2 = [1; sigmoid(Z2)];
Z3 = THETA2 * A2;
h = sigmoid(Z3);
h
alfa = 1;
y = 0;
J = ((y * log(h)) + ((1 - y) * log(1 - h))) * -1 ;
delta3 = h - y;
delta2 = ((THETA2' * delta3) .* (A2 .* (1 - A2)))(2:end);
THETA2 = THETA2 - (alfa * (delta3 * A2'));
THETA1 = THETA1 - (alfa * (delta2 * A1'));
Z2 = THETA1 * A1;
A2 = [1; sigmoid(Z2)];
Z3 = THETA2 * A2;
h = sigmoid(Z3);
h


# funkcja ucząca sieć neuronową funkcji XOR
function [THETA1_new, THETA2_new] = xor_nn(XOR, THETA1, THETA2, init_w=0, learn=0, alpha=0.01)
  # sprawdź, czy to inicjalizacja sieci
  if (init_w == 1)
     THETA1 = 2*rand(2,3) - 1;
     THETA2 = 2*rand(1,3) - 1;
  endif
 
  # sumatory korekcji wag z całego zbioru trenującego
  T1_DELTA = zeros(size(THETA1));
  T2_DELTA = zeros(size(THETA2));
  
  # przejdź przez cały zbiór trenujący
  m = 0;
  
  # funkcja kosztu
  J = 0.0;
  
  #disp('NN output ');
  
  for i = 1:rows(XOR)
    # propagacja sygnału do przodu ku wyjściu
    A1 = [1; XOR(i,1:2)'];
    
    Z2 = THETA1 * A1;
    
    A2 = [1; sigmoid(Z2)];
    
    Z3 = THETA2 * A2;
    
    h = sigmoid(Z3);
    
    J = J + ( XOR(i,3) * log(h) ) + ( (1 - XOR(i,3)) * log(1 - h) );
    
    m = m + 1;

    # liczymy korekcję t2_delta i t1_delta, aby skorygować błąd
    if (learn == 1)
      delta3 = h - XOR(i,3);
      
      delta2 = ((THETA2' * delta3) .* (A2 .* (1 - A2)))(2:end);
         
      # sumuj korekcje dla każdego elementu zbioru uczącego
      T2_DELTA = T2_DELTA + (delta3 * A2');
      T1_DELTA = T1_DELTA + (delta2 * A1');
    else
      disp('Prognoza dla '), disp(XOR(i,1:2)), disp('wynosi '), disp(h);
    endif
  endfor
  
  J = J / -m;
  
  if (learn==1)
    THETA1 = THETA1 - (alpha * (T1_DELTA / m));
    THETA2 = THETA2 - (alpha * (T2_DELTA / m));
  else
    disp('J: '), disp(J);
  endif
  
  THETA1_new = THETA1;
  THETA2_new = THETA2;
  
endfunction

XOR = [0,0,0; 0,1,1; 1,0,1; 1,1,0];

THETA1 = 0;
THETA2 = 0;

[THETA1, THETA2] = xor_nn(XOR, THETA1, THETA2, 1, 1, 0.01);

for i = 1:100000
  [THETA1, THETA2] = xor_nn(XOR, THETA1, THETA2, 0, 1, 0.01); 

  if (mod(i,1000) == 0)
    disp('Iteracja : '), disp(i)
    [THETA1, THETA2] = xor_nn(XOR, THETA1, THETA2);
  endif  
  
endfor

