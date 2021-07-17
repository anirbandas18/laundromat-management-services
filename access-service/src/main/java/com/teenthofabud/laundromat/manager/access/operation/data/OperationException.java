package com.teenthofabud.laundromat.manager.access.operation.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class OperationException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public OperationException(String message) {
        super(message);
    }

    public OperationException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public OperationException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public OperationException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Operation";
    }

}
