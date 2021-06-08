package com.teenthofabud.core.common.data.error;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
public class TOABSystemException extends RuntimeException {

    private transient String message;
    private transient TOABError error;
    private transient Object[] parameters;

    public TOABSystemException(String message) {
        super(message);
        this.message = message;
        this.error = null;
    }

    public TOABSystemException(Throwable throwable) {
        super(throwable);
        this.message = "";
        this.error = null;
    }

    public TOABSystemException(String message, Throwable throwable) {
        super(message, throwable);
        this.message = message;
        this.error = null;
    }

    public TOABSystemException(String message, Object[] parameters) {
        super(message);
        this.message = message;
        this.error = null;
        this.parameters = parameters;
    }

    public TOABSystemException(String message, Object[] parameters, Throwable throwable) {
        super(message, throwable);
        this.message = message;
        this.error = null;
        this.parameters = parameters;
    }

    public TOABSystemException(TOABError error, String message) {
        super(message);
        this.error = error;
        this.message = message;
    }

    public TOABSystemException(TOABError error, String message, Throwable throwable) {
        super(message, throwable);
        this.error = error;
        this.message = message;
    }

    public TOABSystemException(TOABError error, String message, Object[] parameters) {
        super(message);
        this.error = error;
        this.message = message;
        this.parameters = parameters;
    }

    public TOABSystemException(TOABError error, String message, Object[] parameters, Throwable throwable) {
        super(message, throwable);
        this.error = error;
        this.message = message;
        this.parameters = parameters;
    }

    public TOABSystemException(TOABError error, Object[] parameters) {
        this.error = error;
        this.message = "";
        this.parameters = parameters;
    }

    public TOABSystemException(TOABError error, Object[] parameters, Throwable throwable) {
        super(throwable);
        this.error = error;
        this.message = "";
        this.parameters = parameters;
    }

}
