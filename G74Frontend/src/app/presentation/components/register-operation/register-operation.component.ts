import { CommonModule, Time } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { OperationRequest } from '../../../domain/models/operationRequest.model';
import { OperationRequestService } from '../../../application/services/operationRequest.service';


@Component({
  selector: 'app-register-operation',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './register-operation.component.html',
  styleUrl: './register-operation.component.css'
})

export class RegisterOperationComponent {

  specializations: string[] = [''];
  quantities: number[] = [];
  counter: number[] = [];
  priority: string = "";
  medicalRecordNumber: string = "";
  patientName: string = "";
  map: Map<string, number> = new Map<string, number>();
  licenceNumber: string = "";
  operationName: string = "";
  operationDate: string = "";
  operationDuration: string = "";
  
  operationRequest: OperationRequest = {
    medicalRecordNumber: "",
    licenceNumber: "",
    nameOperationType: "",
    requiredStaffSpecialization: [],
    seconds: 0,
    minutes: 0,
    hours: 0,
    days: 0,
    deadlineDate: new Date(),
    priority: ""
  }

  addSpecialization(){
    this.specializations.push('');
    this.quantities.push();
    
  }

  removeSpecialization(){
    this.specializations.pop();
    this.quantities.pop();
  }

  trackByIndex(index: number, obj: any): any {
    return index;
  }

  submitOperation() {
    console.log('Medical Record Number:', this.medicalRecordNumber);
    console.log('Priority:', this.priority);
    console.log('Licence Number:', this.licenceNumber);
    console.log('Operation Name:', this.operationName);
    console.log('Operation Date:', this.operationDate);
    console.log('Operation Duration:', this.operationDuration);




    for (let i = 0; i < this.specializations.length; i++) {
      this.map.set(this.specializations[i], this.quantities[i]);
    }

    this.operationRequest.medicalRecordNumber = this.medicalRecordNumber;
    this.operationRequest.licenceNumber = this.licenceNumber;
    this.operationRequest.nameOperationType = this.operationName;
    this.operationRequest.requiredStaffSpecialization = Array.from(this.map.keys());
    this.operationRequest.seconds = Number(this.operationDuration.split(':')[2]);
    this.operationRequest.minutes = Number(this.operationDuration.split(':')[1]);
    this.operationRequest.hours = Number(this.operationDuration.split(':')[0]);
    this.operationRequest.days = 0;
    this.operationRequest.deadlineDate = new Date(this.operationDate);
    this.operationRequest.priority = this.priority;
    


    console.log('Map:', this.map);
    this.map.clear(); 
  }

}
