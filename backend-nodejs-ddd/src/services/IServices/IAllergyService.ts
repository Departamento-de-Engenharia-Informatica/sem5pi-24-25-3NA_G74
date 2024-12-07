import {IAllergyDTO} from "../../dto/IAllergyDTO";
import {Result} from "../../core/logic/Result";

export default interface IAllergyService  {
    
    createMedicalCondition(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
    UpdateMedicalCondition(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
    SearchMedicalCondition(code?: string): Promise<Result<IAllergyDTO[]>>;

}