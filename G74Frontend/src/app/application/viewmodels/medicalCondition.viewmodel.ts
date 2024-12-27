import { Injectable } from "@angular/core";
import { Observable } from "rxjs";
import { MedicalConditionDto } from "../../dto/medicalCondition.dto";
import { MedicalConditionService } from "../../domain/services/medicalCondition.service";

@Injectable({
    providedIn: 'root'
})


export class MedicalConditionViewModel {

    constructor(private medicalConditionService: MedicalConditionService) { }

    createMedicalCondition(medicalCondition: MedicalConditionDto): Observable<MedicalConditionDto> {
        return this.medicalConditionService.createMedicalCondition(medicalCondition);
    }

    updateMedicalCondition(dto: MedicalConditionDto): Observable<MedicalConditionDto> {

        const code = dto.medicalConditionCode;
        const updatedData = {
            designation: dto.designation,
            description: dto.description
        }

        return this.medicalConditionService.updateMedicalCondition(code, updatedData);
    }


    searchMedicalCondition(medicalConditionCode?: string, designation?: string): Observable<MedicalConditionDto[]> {
        return this.medicalConditionService.searchMedicalCondition(medicalConditionCode, designation);
    }

}