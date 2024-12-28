import { Component, OnInit } from '@angular/core';
import { MedicalRecordDTO } from '../../../dto/medicalRecord.dto';
import { MedicalRecordViewModel } from '../../../application/viewmodels/medicalRecord.viewmodel';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-medical-record-dash',
  templateUrl: './medical-record-dash.component.html',
  styleUrls: ['./medical-record-dash.component.css'],
  standalone: true,
  imports: [FormsModule,CommonModule]
})

export class MedicalRecordDashComponent implements OnInit {
  medicalRecords: MedicalRecordDTO[] = []; // Armazena os registros médicos como um array
  isLoading = false;
  message = '';

  codeFilter = '';
  designatinoFilter = '';

  selectedOption: string = '';
  searchInput: string = '';


  selectedForUpdate: MedicalRecordDTO | null = null;

  constructor(private medicalRecordViewModel: MedicalRecordViewModel) {}

  ngOnInit(): void {
    this.fetchMedicalRecords();
    console.log(this.medicalRecords)
  }

  fetchMedicalRecords(): void {
    this.isLoading = true;
    this.message = "";

    this.medicalRecordViewModel.readMedicalRecord().subscribe({
      next: (response: { records: MedicalRecordDTO[] }) => {
        this.medicalRecords = response.records; 
        console.log(this.medicalRecords);
        this.isLoading = false;
      },
      error: (error) => {
        this.message = "Failed to load medical records.";
        this.isLoading = false;
        console.error('Error fetching medical records:', error);
      }
    });

  }

  trackByIndex(index: number, item: any): any {
    return index;
  }

  editMedicalRecord(record: MedicalRecordDTO): void {
    this.selectedForUpdate = { ...record }; 
  }
  
  cancelEdit(): void {
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
  }
}