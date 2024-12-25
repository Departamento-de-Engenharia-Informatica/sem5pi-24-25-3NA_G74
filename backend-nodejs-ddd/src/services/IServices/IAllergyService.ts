import {IAllergyDTO} from "../../dto/IAllergyDTO";
import {Result} from "../../core/logic/Result";

export default interface IAllergyService  {

    CreateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
    UpdateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>>;
    SearchAllergy(code?: string): Promise<Result<IAllergyDTO[]>>;

}