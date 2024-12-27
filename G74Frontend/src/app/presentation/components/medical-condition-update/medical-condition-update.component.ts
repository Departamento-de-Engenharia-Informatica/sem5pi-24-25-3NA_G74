import {
  Component,
  Input,
  Output,
  EventEmitter,
  OnInit,
  HostListener
} from '@angular/core';
import { MedicalConditionDto } from '../../../dto/medicalCondition.dto';
import { MedicalConditionViewModel } from '../../../application/viewmodels/medicalCondition.viewmodel';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';

@Component({
  selector: 'app-update-medical-condition',
  templateUrl: './medical-condition-update.component.html',
  styleUrls: ['./medical-condition-update.component.css']
})
export class UpdateMedicalConditionComponent implements OnInit {
  @Input() medicalCondition!: MedicalConditionDto;
  @Output() close = new EventEmitter<boolean>();

  isLoading = false;
  message = '';

  constructor(private mcVM: MedicalConditionViewModel) { }

  ngOnInit(): void {
    // You might store an original copy if you want to revert changes.
  }

  update(): void {
    this.isLoading = true;
    this.message = '';

    // For your PATCH route, we only update fields that might have changed
    const updatedDto: MedicalConditionDto = {
      medicalConditionCode: this.medicalCondition.medicalConditionCode,
      designation: this.medicalCondition.designation,
      description: this.medicalCondition.description
      // NOTE: If you want to also allow updating commonSymptoms, add them here
      // commonSymptoms: this.medicalCondition.commonSymptoms
    };

    this.mcVM.updateMedicalCondition(updatedDto)
      .pipe(
        catchError(error => {
          console.error('Error updating Medical Condition:', error);
          this.message = `Failed to update Medical Condition.`;
          this.isLoading = false;
          return of(null);
        })
      )
      .subscribe(response => {
        this.isLoading = false;
        if (response) {
          this.message = 'Medical Condition updated successfully!';
          // Close after a short delay
          setTimeout(() => this.close.emit(true), 1500);
        }
      });
  }

  cancel(): void {
    this.close.emit(false);
  }

  // Optional: close the popup if user clicks outside
  @HostListener('window:click', ['$event.target'])
  onOutsideClick(target: HTMLElement): void {
    if (target.classList.contains('modal-overlay')) {
      this.cancel();
    }
  }
}
