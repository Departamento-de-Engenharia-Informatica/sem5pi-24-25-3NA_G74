

export interface OperationRequest {
  
    medicalRecordNumber: string;
    licenceNumber: string;
    nameOperationType: string;
    requiredStaffSpecialization: string[];
    seconds: number;
    minutes: number;
    hours: number;
    days: number;
    deadlineDate: Date;
    priority: string;
  
  }

