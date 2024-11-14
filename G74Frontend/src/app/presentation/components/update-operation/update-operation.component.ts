import { Component, ViewEncapsulation } from '@angular/core';

import { OperationRequest, OperationRequestDTO } from '../../../domain/models/operationRequest.model';
import { Router } from '@angular/router';
import { OperationRequestService } from '../../../application/services/operationRequest.service';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-update-operation',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './update-operation.component.html',
  styleUrl: './update-operation.component.css',
  encapsulation: ViewEncapsulation.None 
})
export class UpdateOperationComponent {
  operation: OperationRequestDTO;

  constructor(private router: Router, private operationRequestService: OperationRequestService) {
    const navigation = this.router.getCurrentNavigation();
    this.operation = navigation?.extras.state?.['operation'];
    
    this.operationRequest = {
      
      medicalRecordNumber: this.operation?.medicalRecordNumber?.medicalNumber || '',
      licenceNumber: this.operation?.licenceNumber || -1,
      operationTypeId: this.operation?.operationTypeId || -1,
      priority: this.operation?.priority.priorityDescription.toString() || '',
      deadlineDate: this.operation?.deadlineDate.date.toString() || ''
    };
    this.medicalRecordNumber = this.operationRequest.medicalRecordNumber;
    this.licenceNumber = this.operationRequest.licenceNumber;
    this.operationTypeId = this.operationRequest.operationTypeId;
    this.operationDate = this.operationRequest.deadlineDate;
    if(this.operationRequest.priority === '0') {
      this.priority = "ElectiveSurgery";
    }
    if(this.operationRequest.priority === '1') {
      this.priority = "UrgentSurgery";
    }
    if(this.operationRequest.priority === '2') {
      this.priority = "EmergencySurgery";
    }

  }

  priority: string = '';
  medicalRecordNumber: string = '';
  operationType: number = -1;
  licenceNumber: number = 0;
  operationDate: string = '';
  operationTypeId: number = 0;
  
  operationRequest: OperationRequest;

  

  trackByIndex(index: number, obj: any): any {
    return index;
  }

  getPriorityDescription(priority: string): string {
    switch (priority) {
      case '0':
        return 'ElectiveSurgery';
      case '1':
        return 'UrgentSurgery';
      case '2':
        return 'EmergencySurgery';
      default:
        return "";
    }
  }

  submitOperation(): void {
    this.operationRequest.medicalRecordNumber = this.medicalRecordNumber.toString();
    this.operationRequest.licenceNumber = this.licenceNumber;
    this.operationRequest.operationTypeId = this.operationTypeId;
    this.operationRequest.priority = this.priority;
    
    if (typeof this.operationDate === 'string') {
      this.operationRequest.deadlineDate = this.operationDate.toString();
      
    } ;
   

    const observer = {
      next: (response: OperationRequest) => {
        console.log('Operation Request created successfully:', response);
        alert('Operation Request edited successfully');
      },
      error: (error: any) => {
        console.error('Error creating Operation Request:', error);
        alert('Error editing Operation Request');
      },
      complete: () => {
        console.log('Operation Request creation completed');
      }
    };
     this.operationRequestService.updateOperation(this.operationRequest,this.operation.operationRequestId)
       .subscribe(observer);
  }

  ngOnInit(): void {
    if (!this.operation) {  
      this.router.navigate(['/doctor/list-all-operations']);
    }
  }
}
