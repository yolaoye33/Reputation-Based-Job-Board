# 🏆 Reputation-Based Job Board

A decentralized job marketplace built on Stacks blockchain where employers and freelancers build verifiable on-chain reputation through completed work and peer ratings.

## 🌟 Features

- **📝 Job Posting**: Employers can post jobs with automatic escrow
- **🎯 Job Applications**: Freelancers submit proposals for open positions  
- **🔒 Secure Escrow**: Funds held in smart contract until job completion
- **⭐ Rating System**: Mutual rating system for reputation building
- **📊 Reputation Scores**: Algorithmic reputation calculation based on ratings and completion history
- **👤 User Profiles**: Track earnings, spending, and performance metrics

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <repository-url>
cd reputation-job-board
clarinet check
```

## 📖 Usage Guide

### For Employers 👔

1. **Create Profile**
```clarity
(contract-call? .reputation-based-job-board create-profile)
```

2. **Post a Job**
```clarity
(contract-call? .reputation-based-job-board post-job 
  "Frontend Developer" 
  "Build a React dashboard with Web3 integration" 
  u1000000)
```

3. **Assign Job to Freelancer**
```clarity
(contract-call? .reputation-based-job-board assign-job u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

4. **Complete Job & Release Payment**
```clarity
(contract-call? .reputation-based-job-board complete-job u1)
```

5. **Rate Freelancer**
```clarity
(contract-call? .reputation-based-job-board rate-user 
  u1 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 
  u5 
  "Excellent work quality and communication")
```

### For Freelancers 💼

1. **Create Profile**
```clarity
(contract-call? .reputation-based-job-board create-profile)
```

2. **Apply for Job**
```clarity
(contract-call? .reputation-based-job-board apply-for-job 
  u1 
  "I have 5 years React experience and can deliver in 2 weeks")
```

3. **Rate Employer After Completion**
```clarity
(contract-call? .reputation-based-job-board rate-user 
  u1 
  'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE 
  u4 
  "Clear requirements and prompt payment")
```

## 🔍 Query Functions

### Get Job Details
```clarity
(contract-call? .reputation-based-job-board get-job u1)
```

### Check User Reputation
````clarity
(contract-call? .reputation-based-job-board get
