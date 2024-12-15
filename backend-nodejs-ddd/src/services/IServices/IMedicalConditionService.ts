import { Result } from "../../core/logic/Result";
import { IMedicalConditionDTO } from "../../dto/IMedicalConditionDTO";


export default interface IMedicalConditionService  {

    createMedicalCondition(medicalConditionDTO: IMedicalConditionDTO): Promise<Result<IMedicalConditionDTO>>;
    UpdateMedicalCondition(medicalConditionDTO: IMedicalConditionDTO): Promise<Result<IMedicalConditionDTO>>;
    SearchMedicalCondition(medicalConditionCode?:string, designation?: string): Promise<Result<IMedicalConditionDTO[]>>;

}