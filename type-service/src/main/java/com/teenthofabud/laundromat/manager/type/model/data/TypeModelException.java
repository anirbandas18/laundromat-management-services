package com.teenthofabud.laundromat.manager.type.model.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TypeModelException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public TypeModelException(String message) {
        super(message);
    }

    public TypeModelException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public TypeModelException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public TypeModelException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Model";
    }

}
