package com.teenthofabud.laundromat.manager.type.error;

import com.teenthofabud.core.common.data.error.TOABBaseException;
import com.teenthofabud.core.common.data.error.TOABError;
import com.teenthofabud.laundromat.manager.type.constant.TypeSubDomain;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TypeException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;
    @ToString.Include
    private transient TypeSubDomain type;

    public TypeException(TypeSubDomain type, String message) {
        super(message);
        this.type = type;
    }

    public TypeException(TypeSubDomain type, String message, Object[] parameters) {
        super(message, parameters);
        this.type = type;
    }

    public TypeException(TypeSubDomain type, TOABError error, String message, Object[] parameters) {
        super(error, type, message, parameters);
        this.error = error;
        this.type = type;
    }

    public TypeException(TypeSubDomain type, TOABError error, Object[] parameters) {
        super(error, type, parameters);
        this.error = error;
        this.type = type;
    }

}
