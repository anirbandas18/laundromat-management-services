package com.teenthofabud.core.common.model.error;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class TOABBaseException extends Exception {

    private String message;
    private TOABError error;
    private Object[] parameters;

    public TOABBaseException(String message) {
        this.message = message;
        this.error = null;
    }

    public TOABBaseException(String message, Object[] parameters) {
        this.message = message;
        this.error = null;
        this.parameters = parameters;
    }

    public TOABBaseException(TOABError error, String message, Object[] parameters) {
        this.error = error;
        this.message = message;
        this.parameters = parameters;
    }

    public TOABBaseException(TOABError error, Object[] parameters) {
        this.error = error;
        this.message = "";
        this.parameters = parameters;
    }

}
