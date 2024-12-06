import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Entity} from "../core/domain/Entity";

export class AllergyId extends Entity<any>{
    
    get id (): UniqueEntityID{
        return this._id;
    }
    
    private constructor(id?: UniqueEntityID) {
        super(null, id);
    }
    
}