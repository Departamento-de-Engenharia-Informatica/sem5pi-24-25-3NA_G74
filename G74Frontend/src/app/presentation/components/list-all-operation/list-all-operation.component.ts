import { Component, ViewEncapsulation } from '@angular/core';
import { OperationRequest, OperationRequestDTO } from '../../../domain/models/operationRequest.model';
import {OperationRequestService} from '../../../application/services/operationRequest.service';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { ConfirmDialogComponent } from '../confirm-dialog/confirm-dialog.component';
import { MatDialog } from '@angular/material/dialog';

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

  constructor(private router: Router,private operationService: OperationRequestService, private dialog:MatDialog) {}

  redirectToRegister() {
    this.router.navigate(['/doctor/create-operation']);
  }

  redirectToEdit(operation: OperationRequestDTO) {
    this.router.navigate(['/doctor/update-operation'], { state: { operation } });
  }

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

  deleteOperation(operation: OperationRequestDTO): void {
    const dialogRef = this.dialog.open(ConfirmDialogComponent, {
      data: { message: 'Are you sure you want to delete this operation?' }
    });
    dialogRef.afterClosed().subscribe(result => {
      if (result) {
        this.operationService.deleteOperation(operation.operationRequestId).subscribe(() => {
          this.getOperations();
        });
      }
    });
  }

}
