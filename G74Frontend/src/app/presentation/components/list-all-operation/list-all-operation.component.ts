import { Component, ViewEncapsulation } from '@angular/core';
import { OperationRequest, OperationRequestDTO } from '../../../domain/models/operationRequest.model';
import {OperationRequestService} from '../../../application/services/operationRequest.service';
import { CommonModule } from '@angular/common';
@Component({
  selector: 'app-list-all-operation',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './list-all-operation.component.html',
  styleUrl: './list-all-operation.component.css',
  encapsulation: ViewEncapsulation.None 
})
export class ListAllOperationComponent {
  operations: OperationRequestDTO[] = [];

  constructor(private operationService: OperationRequestService) {}

  ngOnInit(): void {
    this.getOperations();
  }

  getOperations(): void {
    this.operationService.listAllOperations().subscribe((operations) => {
      this.operations = operations;
      
      console.log(operations);  
    });
  }

  trackByIndex(index: number, item: any): any {
    return index;
  }
  getPriorityDescription(priority: number): string {
    switch (priority) {
      case 0:
        return 'Elective';
      case 1:
        return 'Urgent';
      case 2:
        return 'Emergency';
      default:
        return 'Unknown';
    }
  }

}
