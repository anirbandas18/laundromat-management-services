package com.teenthofabud.laundromat.manager.type.model.error;

import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.core.common.model.error.TOABError;
import com.teenthofabud.laundromat.manager.type.model.constants.LOVType;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TypeException extends TOABBaseException {

    private TOABError error;
    private LOVType type;

    public TypeException(LOVType type, String message) {
        super(message);
        this.type = type;
    }

    public TypeException(LOVType type, String message, Object[] parameters) {
        super(message, parameters);
        this.type = type;
    }

    public TypeException(LOVType type, TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
        this.type = type;
    }

    public TypeException(LOVType type, TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
        this.type = type;
    }

}
