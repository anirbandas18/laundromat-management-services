package com.teenthofabud.laundromat.manager.type.lov.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TypeLOVException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public TypeLOVException(String message) {
        super(message);
    }

    public TypeLOVException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public TypeLOVException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public TypeLOVException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "LOV";
    }

}
