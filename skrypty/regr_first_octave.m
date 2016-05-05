%% Sztuczna Inteligencja - liniowa regresja


%% Inicjalizacja
clear all; close all; clc

function plotData(x, y)
figure; % otwiera nowe okno
% rysuje wykres
plot(x, y, 'rx', 'MarkerSize', 10);
% dodaje etykiety x i y osi
ylabel('Profit in $10,000s');
xlabel('Population of City in 10,000s');
end

function [theta, J_history] = gradientDescent(X, y, theta, alpha, num_iters)
%GRADIENTDESCENT liczy spadek gradientu i znajduje optymalne parametry
%   theta = GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by 
%   taking num_iters gradient steps with learning rate alpha

% Inicjalizacja
m = length(y); % ilość przykładów trenujących
J_history = zeros(num_iters, 1);

for iter = 1:num_iters
    %h(x(i)) 
    hipoteses = X * theta;

    %wektor theta ustawianie
    theta0 = theta(1) - alpha / m * sum((hipoteses - y) .* X(:,1));
    theta1 = theta(2) - alpha / m * sum((hipoteses - y) .* X(:,2));
    theta = [theta0; theta1];
    
    % zapisz koszt J w każdej iteracji
    J_history(iter) = computeCost(X, y, theta);
end

end

function J = computeCost(X, y, theta)
%COMPUTECOST oblicz koszt liniowej regresji 
%J = COMPUTECOST(X, y, theta) używa theta 
%do znalezienia prostej regresji biegnącej przez wszystkie punkty

% Inicjalizacja
m = length(y); % ilość przykładów trenujących

 
J = 0;
soma = 0;
tmp = 0;

%w pętli co jest bez sensu w macierzowym języku
%for i=1:m, 
%	% formula: x(i) * theta - y(i)
%	suma = suma + (X(i,:) * theta - y(i))^2;
%end
%J = suma/(2*m);

hipoteses = X * theta;
desviopadrao = (hipoteses - y).^2;
J = 1 / (2*m) * sum(desviopadrao);
%sprintf("Theta %0.2f -> J %0.2f",theta(1),J)
end



%% ======================= Part 2: Plotting =======================
fprintf('Plotting Data ...\n')
data = load('ex1data1.txt');
X = data(:, 1); y = data(:, 2);
m = length(y); % ilość przykładów trenujących

plotData(X, y);

fprintf('Pausa. naciśnij enter.\n');
%pause;

%% =================== Part 3: Gradient descent ===================
fprintf('Spadek gradientu ...\n')

X = [ones(m, 1), data(:,1)]; % dodaj kolumnę 1-ek do X
theta = zeros(2, 1); % inicjalizacja theta

iterations = 1500;
alpha = 0.01;

% koszt wstępny
computeCost(X, y, theta)

theta = gradientDescent(X, y, theta, alpha, iterations);

% wypisz theta
fprintf('Theta found by gradient descent: ');
fprintf('%f %f \n', theta(1), theta(2));

% Plot the linear fit
hold on; % gotowy do dodania
plot(X(:,2), X*theta, '-')
legend('Training data', 'Linear regression')
hold off % już nic nie dodawaj do rysunku

% Predict values for population sizes of 35,000 and 70,000
predict1 = [1, 3.5] *theta;
fprintf('For population = 35,000, we predict a profit of %f\n',...
    predict1*10000);
predict2 = [1, 7] * theta;
fprintf('For population = 70,000, we predict a profit of %f\n',...
    predict2*10000);

fprintf('Pausa. naciśnij enter.\n');
%pause;

%% ============= Part 4: Wizualizacja J(theta_0, theta_1) =============
fprintf('Visualizing J(theta_0, theta_1) ...\n')

% siatka parametrów theta dla J
theta0_vals = linspace(-10, 10, 100);
theta1_vals = linspace(-1, 4, 100);

% inicjalizacja
J_vals = zeros(length(theta0_vals), length(theta1_vals));

% wypełnij wartości J
for i = 1:length(theta0_vals)
    for j = 1:length(theta1_vals)
	  t = [theta0_vals(i); theta1_vals(j)];    
	  J_vals(i,j) = computeCost(X, y, t);
    end
end


%transponujemy aby w surf były dobrze położone osie
J_vals = J_vals';
% powierzchnia J w wymiarach theta
figure;
surf(theta0_vals, theta1_vals, J_vals)
xlabel('\theta_0'); ylabel('\theta_1');

% poziomice J
figure;
% rysuj J_vals 15 konturów poziomic rozłożonych 
% logarytmicznie pomiędzy 0.01 i 100
contour(theta0_vals, theta1_vals, J_vals, logspace(-2, 3, 20))
xlabel('\theta_0'); ylabel('\theta_1');
hold on;
plot(theta(1), theta(2), 'rx', 'MarkerSize', 10, 'LineWidth', 2);