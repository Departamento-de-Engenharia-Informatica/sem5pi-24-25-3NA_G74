import { MedicalCondition } from "../domain/models/medicalCondition.model";
import { MedicalConditionDto } from "../dto/medicalCondition.dto";


export class MedicalConditionMapper {

    public static toDomain(medicalConditionDto: MedicalConditionDto): MedicalCondition {
        
        const designation = medicalConditionDto.designation? medicalConditionDto.designation : "UNAVAILABLE_DESIGNATION";

        const description = medicalConditionDto.description? medicalConditionDto.description : "UNAVAILABLE_DESCRIPTION";

        const commonSymptoms = medicalConditionDto.commonSymptoms? medicalConditionDto.commonSymptoms : "UNAVAILABLE_SYMPTOMS";
                
        return {
            medicalConditionCode: medicalConditionDto.medicalConditionCode,
            designation: designation,
            description: description,
            commonSymptoms: commonSymptoms
        };
        
    }

    public static toDto(medicalCondition: MedicalCondition): MedicalConditionDto {
        return {
            medicalConditionCode: medicalCondition.medicalConditionCode,
            designation: medicalCondition.designation,
            description: medicalCondition.description,
            commonSymptoms: medicalCondition.commonSymptoms
        };
    }



}