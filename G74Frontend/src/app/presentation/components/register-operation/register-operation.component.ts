import { CommonModule, Time } from '@angular/common';
import { Component, ViewEncapsulation } from '@angular/core';
import { FormBuilder, FormGroup, FormsModule, Validators } from '@angular/forms';
import { OperationRequest } from '../../../domain/models/operationRequest.model';
import { OperationRequestService } from '../../../application/services/operationRequest.service';

@Component({
  selector: 'app-register-operation',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './register-operation.component.html',
  styleUrl: './register-operation.component.css',
  encapsulation: ViewEncapsulation.None
})

export class RegisterOperationComponent {
  operationForm: FormGroup | undefined;
  constructor(private operationRequestService: OperationRequestService, private fb:FormBuilder) { }

  priority: string = "";
  medicalRecordNumber: string = "";
  operationType: number = -1;
  licenceNumber: number = 0;
  operationDate: string = "";
  operationTypeId: number = 0;

  operationRequest: OperationRequest = {
    medicalRecordNumber: "",
    licenceNumber: 0,
    operationTypeId: -1,
    priority: "",
    deadlineDate: ""
  }
  



  trackByIndex(index: number, obj: any): any {
    return index;
  }

  submitOperation(): void {
    
      this.operationRequest.medicalRecordNumber = this.medicalRecordNumber.toString();
      this.operationRequest.licenceNumber = this.licenceNumber;
      this.operationRequest.operationTypeId = this.operationTypeId;
      this.operationRequest.priority = this.priority;

      if (typeof this.operationDate === 'string') {
        this.operationRequest.deadlineDate = this.operationDate;

      } ;
      console.log(this.operationRequest);

      const observer = {
        next: (response: OperationRequest) => {
          console.log('Operation Request created successfully:', response);
          alert('Operation Request created successfully');
        },
        error: (error: any) => {
          console.error('Error creating Operation Request:', error);
          alert('Error creating Operation Request');
        },
        complete: () => {
          console.log('Operation Request creation completed');
        }
      };
      this.operationRequestService.createOperation(this.operationRequest)
        .then(observer.next)
        .catch(observer.error)
        .finally(observer.complete);

    this.medicalRecordNumber = "";
    this.licenceNumber = 0;
    this.operationTypeId = 0;
    this.operationDate = "";
    this.priority = "";

  }
}
