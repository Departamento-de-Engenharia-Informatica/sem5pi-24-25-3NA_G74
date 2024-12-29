import { Injectable } from "@angular/core";
import { MedicalRecordService } from "../../domain/services/medicalRecord.service";
import { MedicalRecordDTO } from "../../dto/medicalRecord.dto";
import { Observable } from "rxjs";

@Injectable({
    providedIn: 'root'
})

export class MedicalRecordViewModel {

    constructor(private medicalRecordService: MedicalRecordService) { }

    // createMedicalCondition(medicalCondition: MedicalConditionDto): Observable<MedicalConditionDto> {
    //     return this.medicalConditionService.createMedicalCondition(medicalCondition);
    // }

    // updateMedicalCondition(dto: MedicalConditionDto): Observable<MedicalConditionDto> {
    //     return this.medicalConditionService.updateMedicalCondition(dto);
    // }


    // searchMedicalCondition(medicalConditionCode?: string, designation?: string): Observable<MedicalConditionDto[]> {
    //     return this.medicalConditionService.searchMedicalCondition(medicalConditionCode, designation);
    // }

    readMedicalRecord(){
        return this.medicalRecordService.getMedicalRecords();
    }

    searchMedicalRecordByMedicalCondition(medicalCondition: string){
        return this.medicalRecordService.searchByMedicalCondition(medicalCondition);
    }
    searchMedicalRecordByAllergy(allergy: string){
        return this.medicalRecordService.searchByAllergies(allergy);
    }
    searchMedicalRecordByPatientId(id: string){
        return this.medicalRecordService.searchByPatientId(id);
    }
    create(medicalRecordDTO: MedicalRecordDTO): Observable<MedicalRecordDTO>{
        return this.medicalRecordService.create(medicalRecordDTO);
    }

}