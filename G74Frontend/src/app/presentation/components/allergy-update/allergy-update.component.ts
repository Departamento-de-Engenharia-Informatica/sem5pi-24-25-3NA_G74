import {Component, EventEmitter, HostListener, Input, OnInit, Output} from '@angular/core';
import {catchError} from 'rxjs/operators';
import {of} from 'rxjs';
import {AllergyDTO} from '../../../dto/allergy.dto';
import {AllergyViewModel} from '../../../application/viewmodels/allergy.viewmodel';

@Component({
  selector: 'app-allergy-update',
  templateUrl: './allergy-update.component.html',
  styleUrl: './allergy-update.component.css'
})
export class AllergyUpdateComponent implements OnInit{
  @Input() allergy!: AllergyDTO;
  @Output() close = new EventEmitter<boolean>();

  isLoading = false;
  message = '';

  constructor(private allergyVM: AllergyViewModel) {}

  ngOnInit(): void {
  }

  update(): void {
    this.isLoading = true;
    this.message = '';

    const updatedDto: AllergyDTO = {
      code: this.allergy.code,
      designation: this.allergy.designation,
      description: this.allergy.description
    };

    this.allergyVM.updateAllergy(updatedDto)
      .pipe(
        catchError(error => {
          console.error('Error updating allergy:', error);
          this.message = `Failed to update allergy.`;
          this.isLoading = false;
          return of(null);
        })
      )
      .subscribe(response => {
        this.isLoading = false;
        if (response) {
          this.message = 'Allergy updated successfully!';
          setTimeout(() => this.close.emit(true), 1500);
        }
      });
  }

  cancel(): void {
    this.close.emit(false);
  }

  @HostListener('window:click', ['$event.target'])
  onOutsideClick(target: HTMLElement): void {
    if (target.classList.contains('modal-overlay')) {
      this.cancel();
    }
  }
}
