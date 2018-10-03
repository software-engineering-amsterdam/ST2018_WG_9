## Workshop Group 9 - Guest Lecture Axini
### 1. Are all test cases passed?
Door #1: Yes  
Door #2: No
Door #3: No, out of the 5 test cases, the last 3 failed.
Door #4: No, out of the 5 test cases, 4 of them have failed.
Door #5: No, out of the 5 test cases, 2 of them have failed

### 2. Click on one of the test cases. Study the messages that go to the system and are snet back from it. In the test case that did not pass, describe as brief and percise as possible why the implementation doen not uphold the specification.
Door #1: passed  
Door #2: Unable to connect to the system. Therefore we cannot test it
Door #3: Stimuli ?open expects the door to be !open. After the door is given the stimuli ?close, it asserts again if the door is !opened. This shows up in the test case as an unexpected response.
Door #4: Stumuli ?close expects the door to be !closed, however, there is an unexpected or late response here that fails the test case.
Door #5: 

### 3. Go to the "Coverage" page. Is the specification completely covered?
Door #1: 100% state coverage
Door #2: see answer at 2
Door #3: 100% Stat coverage, 90% transition coverage
Door #4: 100% State coverage, 70% transition coverage
Door #5: 100% State coverage, 100% transition coverage

### 4. Is this the correct implementation of the specificatiom?
Door #1: yes
Door #2: see answer at 2
Door #3: No, see answer at 2
Door #4: No, see answer at 2
Door #5: No, see answer at 2

### 5. 'Fix' the model, such that it passes the test. Note that one normally would fix the implementation for most of these errors. But in this exercise we fix the model.
Door #1: Not needed for Door #1.
Door #2: See answer at 2.
Door #3: State opened if stimuli ?close is received must go to opened state.
Door #4: Change all illegals to !closed (4 places)
Door #5: 
