import { Component, ViewEncapsulation } from '@angular/core';
import { OperationRequestDTO } from '../../../domain/models/operationRequest.model';
import { OperationRequestService } from '../../../application/services/operationRequest.service';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-delete-operation',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './delete-operation.component.html',
  styleUrl: './delete-operation.component.css',
  encapsulation: ViewEncapsulation.None 
})
export class DeleteOperationComponent {
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
