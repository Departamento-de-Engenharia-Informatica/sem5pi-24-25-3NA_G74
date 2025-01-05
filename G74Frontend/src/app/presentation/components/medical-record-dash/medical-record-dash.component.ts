import { Component, Injectable, OnInit } from '@angular/core';
import { MedicalRecordDTO } from '../../../dto/medicalRecord.dto';
import { MedicalRecordViewModel } from '../../../application/viewmodels/medicalRecord.viewmodel';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { PatientViewModel } from '../../../application/viewmodels/patient-viewmodel';
import { Patient } from '../../../domain/models/patient.model';
import { response } from 'express';
import { catchError, map, of, tap } from 'rxjs';
import { MedicalConditionViewModel } from '../../../application/viewmodels/medicalCondition.viewmodel';
import { MedicalConditionDto } from '../../../dto/medicalCondition.dto';
import { AuthService } from '../../../domain/services/auth.service';
import { AllergyViewModel } from '../../../application/viewmodels/allergy.viewmodel';
import { AllergyDTO } from '../../../dto/allergy.dto';
import { MatDialog } from '@angular/material/dialog';
import { ConfirmDialogComponent } from '../confirm-dialog/confirm-dialog.component';

@Component({
  selector: 'app-medical-record-dash',
  templateUrl: './medical-record-dash.component.html',
  styleUrls: ['./medical-record-dash.component.css'],
  standalone: true,
  imports: [FormsModule,CommonModule]
})


export class MedicalRecordDashComponent implements OnInit {
  medicalRecords: MedicalRecordDTO[] = []; // Armazena os registros médicos como um array
  medicalConditions: MedicalConditionDto[] = [];
  patients: Patient[] = [];
  allergies: AllergyDTO[] = [];
  filters: Partial<Patient> = {};

  medicalRecord: MedicalRecordDTO = {
    medicalRecordCode: '',
    medicalConditions: [],
    allergies: [],
    freeText: ''
  };


  phoneNumberFilter: string = '';
  emailAddressFilter: string = '';
  dateOfBirthInput: string = '';
  message: string = '';
  medicalRecordNumber: string = '';
  isLoading: boolean = false;
  selectedPatientForUpdate: Patient | null = null; // Patient to update
  selectedPatientForDelete: Patient | null = null; // Patient to delete
  codeFilter = '';
  designationFilter = '';
  selectedOption: string = '';
  searchInput: string = '';
  showAddForm = false;
  showEditForm = false;


  newMedicalRecord: MedicalRecordDTO = {
    medicalRecordCode: '',
    medicalConditions: [],
    allergies: [],
    freeText: ''
  }

  get isDoctor(): boolean {
    // If the stored user role is 'Admin', return true
    return this.authService.currentUserSubject.value?.role === 'Doctor';
  }

  selectedForUpdate: MedicalRecordDTO | null = null;

  constructor(private medicalRecordViewModel: MedicalRecordViewModel,
              private patientViewModel: PatientViewModel,
              private medicalConditionVM: MedicalConditionViewModel,
              private authService: AuthService,
              private allergyVM: AllergyViewModel, 
              private dialog: MatDialog
            ) {}
            

  ngOnInit(): void {
    this.fetchMedicalRecords();
    this.fetchPatients();
    this.fetchMedicalConditions();
    console.log(this.medicalConditions)
    this.fetchAllergy();
  }

  onSubmit(): void {
    
    this.message = '';
    console.log(this.medicalRecord);

    const dialogRef = this.dialog.open(ConfirmDialogComponent, {
      data: { message: 'Are you sure you want to add this new medical record?' }
    });
    dialogRef.afterClosed().subscribe(result => {
      if (result) {
        this.isLoading = true;
        this.medicalRecordViewModel
      .create(this.medicalRecord)
      .pipe(
        catchError(error => {
          console.error('Error creating Medical Record:', error);
          this.isLoading = false;
          return of(null);
        })
      )
      .subscribe(response => {
        this.isLoading = false;
        if (response) {
          this.message = 'Medical Record created successfully!';
          this.resetForm();
          location.reload();
        }
      });
      }else{
        this.isLoading = false;
      }
    });
    
  }

  fetchAllergy(): void {
    this.isLoading = true;
    this.message = '';

    this.allergyVM.searchAllergy(this.codeFilter, this.designationFilter)
      .pipe(
        catchError(error => {
          console.error('Error searching allergy:', error);
          this.message = 'Failed to fetch allergy.';
          this.isLoading = false;
          return of([]);
        })
      )
      .subscribe((results) => {
        this.allergies = results;
        console.log("Allergies: "+ this.allergies.length);
        this.allergies.push(
          {
            code: '123',
            designation: 'Eggs'
          },
          {
            code: '1234',
            designation: 'Red Meat'
          }
      )
        this.isLoading = false;
        if (!Array.isArray(results) || !results.length) {
          this.message = 'No allergy found.';
        }
      });
  }

  fetchMedicalRecords(): void {
    this.isLoading = true;
    this.message = '';

    this.medicalRecordViewModel.readMedicalRecord().subscribe({
      next: (response: { isSuccess: boolean; isFailure: boolean; error: any; _value: MedicalRecordDTO[] }) => {
        if (response.isSuccess && Array.isArray(response._value)) {
          this.medicalRecords = response._value;
          console.log('Fetched medical records:', this.medicalRecords);
        } else {
          this.medicalRecords = [];
          this.message = 'No medical records found.';
        }
        this.isLoading = false;
      },
      error: (error) => {
        this.message = "Failed to load medical records.";
        this.isLoading = false;
        console.error('Error fetching medical records:', error);
      }
    });
}

  fetchPatients(): void {
    this.isLoading = true;

    const filters: Partial<Patient> = { ...this.filters };

    this.patientViewModel
      .listPatients(Object.keys(filters).length ? filters : null)
      .pipe(
        catchError((error) => {
          console.error('Error fetching patients:', error);
          this.message = 'Failed to fetch patients. Please try again.';
          this.isLoading = false;
          return of([]);
        })
      )
      .subscribe((patients) => {
        this.patients = patients;
        console.log(this.patients)
        this.message = patients.length ? '' : 'No patients found.';
        this.isLoading = false;
      });
  }

  fetchMedicalConditions(): void {
    this.isLoading = true;
    this.message = '';

    this.medicalConditionVM.searchMedicalCondition(this.codeFilter, this.designationFilter)
      .pipe(
        catchError(error => {
          console.error('Error searching medical conditions:', error);
          this.message = 'Failed to fetch medical conditions.';
          this.isLoading = false;
          return of([]);
        })
      )
      .subscribe((results) => {
        this.medicalConditions = results;
        this.medicalConditions.push({
          medicalConditionCode: '123',
          designation: 'Diabetes'
        },
        {
          medicalConditionCode: '1234',
          designation: 'Asma'
        },
        {
          medicalConditionCode: '12345',
          designation: 'Anxiety Disorders'
        },
        {
          medicalConditionCode: '123456',
          designation: 'Arrhythmias'
        },
      )
        console.log(this.medicalConditions);
        this.isLoading = false;
        if (!results || !results.length) {
          this.message = 'No medical conditions found.';
        }
      });
  }

  trackByIndex(index: number, item: any): any {
    return index;
  }

  editMedicalRecord(record: MedicalRecordDTO): void {
    this.selectedForUpdate = { ...record }; 
    console.log(this.medicalRecordViewModel.edit(record.medicalRecordCode, record))
  }

  async update(): Promise<void> {
    this.isLoading = true;
    this.message = '';

    let updatedDto: MedicalRecordDTO = {
      medicalRecordCode: this.selectedForUpdate?.medicalRecordCode || '',
      allergies: this.selectedForUpdate?.allergies || [],
      medicalConditions: this.selectedForUpdate?.medicalConditions || [],
      freeText: this.selectedForUpdate?.freeText || ''
    };

    const dialogRef = this.dialog.open(ConfirmDialogComponent, {
      data: { message: 'Are you sure you want to update this medical record?' }
    });

    dialogRef.afterClosed().subscribe(result => {
      if (result) {
        if (this.medicalRecordViewModel && updatedDto.medicalRecordCode) {
          this.medicalRecordViewModel.edit(this.medicalRecord.medicalRecordCode, updatedDto)
            .pipe(
              catchError(error => {
                console.error('Error updating Medical Record:', error);
                this.message = 'Failed to update Medical Record.';
                this.isLoading = false;
                return of(null);
              })
            )
            .subscribe(response => {
              this.isLoading = false;
              if (response) {
                this.message = 'Medical Record updated successfully!';
                this.cancelEdit();
                location.reload();
              }
            });
        }
      } else {
        this.isLoading = false;
      }
    });
  }

    
    
  
  cancelEdit(): void {
    this.showEditForm = false;
    this.selectedForUpdate = null; 
  }

  deleteMedicalRecord(patientId: string): void {
    
  }

  onSearch(){
    this.isLoading = true;
    console.log('Selected Option: ', this.selectedOption);
    console.log('Search Input: ', this.searchInput);
    if(this.selectedOption === 'medical condition'){
      this.medicalRecordViewModel.searchMedicalRecordByMedicalCondition(this.searchInput).subscribe({
        next: (response: { records: MedicalRecordDTO[] }) => {
          this.medicalRecords = response.records;
          console.log(this.medicalRecords);
          this.isLoading = false;
        },
        error: (error) => {
          this.message = "Failed to load medical records.";
          this.isLoading = false;
          this.medicalRecords=[];
          console.log("Error fetching medical records: ", error)
        }
      })
      
    }
    if(this.selectedOption === 'allergy'){
      this.medicalRecordViewModel.searchMedicalRecordByAllergy(this.searchInput).subscribe({
        next: (response: { records: MedicalRecordDTO[] }) => {
          this.medicalRecords = response.records;
          console.log(this.medicalRecords);
          this.isLoading = false;
        },
        error: (error) => {
          this.message = "Failed to load medical records.";
          this.isLoading = false;
          this.medicalRecords=[];
          console.log("Error fetching medical records: ", error)
        }
      })
    }
    if (this.selectedOption === 'patientId') {
      this.medicalRecordViewModel.searchMedicalRecordByPatientId(this.searchInput).pipe(
        tap(response => console.log("Raw response: ", response)),
        map((response: any) => {
          const record = response.record.record; 
          return {
            medicalRecordCode: record.patientId,
            medicalConditions: record.medicalConditions,
            allergies: record.allergies,
            freeText: record.freeText,
          } as MedicalRecordDTO;
        })
      ).subscribe({
        next: (transformedResponse: MedicalRecordDTO) => {
          this.medicalRecords = [transformedResponse]; 
          console.log(this.medicalRecords);
          this.isLoading = false; 
        },
        error: (error) => {
          this.message = "Failed to load medical records.";
          this.isLoading = false;
          this.medicalRecords = [];
          console.error("Error fetching medical records: ", error);
        }
      });
    }
  }

  toggleAddForm(): void {
    this.showAddForm = !this.showAddForm;
  }

  toggleEditForm(): void {
    this.showEditForm = !this.showEditForm;
  }

  addMedicalRecord(): void {
    this.medicalRecords.push({ ...this.newMedicalRecord });
    this.newMedicalRecord = {
      medicalRecordCode: '',
      medicalConditions: [],
      allergies: [],
      freeText: ''
    };
    this.showAddForm = false;
  }

  resetForm(): void {
    this.medicalRecord = {
      medicalRecordCode: '',
      medicalConditions: [],
      allergies: [],
      freeText: ''
    };
  }

  onMedicalConditionsChange(event: Event): void {
    const select = event.target as HTMLSelectElement;
    this.medicalRecord.medicalConditions = Array.from(select.selectedOptions, option => option.value);
  }

  onAllergiesChange(event: Event): void {
    const select = event.target as HTMLSelectElement;
    this.medicalRecord.allergies = Array.from(select.selectedOptions, option => option.value);
  }
}