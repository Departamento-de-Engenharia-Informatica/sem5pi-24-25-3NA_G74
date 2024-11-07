

export interface OperationRequest {

    medicalRecordNumber: string;
    licenceNumber: string;
    nameOperationType: string;
    requiredStaffSpecialization: string[];
    seconds: BigInt;
    minutes: BigInt;
    hours: BigInt;
    days: BigInt;
    deadlineDate: Date;
    priority: string;
  
  }

