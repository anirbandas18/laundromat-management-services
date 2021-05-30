package com.teenthofabud.core.common.model.error;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
public abstract class SystemException extends RuntimeException {

    private transient String message;
    private transient TOABError error;
    private transient Object[] parameters;

    public SystemException(String message) {
        this.message = message;
        this.error = null;
    }

    public SystemException(String message, Object[] parameters) {
        this.message = message;
        this.error = null;
        this.parameters = parameters;
    }

    public SystemException(TOABError error, String message, Object[] parameters) {
        this.error = error;
        this.message = message;
        this.parameters = parameters;
    }

    public SystemException(TOABError error, Object[] parameters) {
        this.error = error;
        this.message = "";
        this.parameters = parameters;
    }

}
