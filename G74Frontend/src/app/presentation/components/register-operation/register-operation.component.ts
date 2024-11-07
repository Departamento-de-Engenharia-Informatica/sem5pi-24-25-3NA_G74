import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { OperationRequest } from '../../../domain/models/operationRequest.model';
@Component({
  selector: 'app-register-operation',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './register-operation.component.html',
  styleUrl: './register-operation.component.css'
})
export class RegisterOperationComponent {
  specializations: string[] = [''];
  counter: number[] = [];
  priority: string = "";

  addSpecialization(){
    this.specializations.push('');
    this.counter.push();
  }

  removeSpecialization(){
    this.specializations.pop();
    this.counter.pop();
  }
  trackByIndex(index: number, obj: any): any {
    return index;
  }
}
