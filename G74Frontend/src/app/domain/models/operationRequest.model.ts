

export interface OperationRequest {
    medicalRecordNumber: string;
    licenceNumber: number;
    operationTypeId: number;
    deadlineDate: string;
    priority: string;
  }

  export interface OperationRequestDTO{
    operationRequestId: number;
    medicalRecordNumber: {
      medicalNumber: string;
    };
    licenceNumber: number;
    operationTypeId: number;
    priority: {
      priorityDescription: number;
    };
    deadlineDate: {
      date: Date;
    };
  }

