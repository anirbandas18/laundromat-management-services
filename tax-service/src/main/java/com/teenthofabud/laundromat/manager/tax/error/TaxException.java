package com.teenthofabud.laundromat.manager.tax.error;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import com.teenthofabud.laundromat.manager.tax.constant.TaxSubDomain;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TaxException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;
    @ToString.Include
    private transient TaxSubDomain type;

    public TaxException(TaxSubDomain type, String message) {
        super(message);
        this.type = type;
    }

    public TaxException(TaxSubDomain type, String message, Object[] parameters) {
        super(message, parameters);
        this.type = type;
    }

    public TaxException(TaxSubDomain type, TOABError error, String message, Object[] parameters) {
        super(error, type, message, parameters);
        this.error = error;
        this.type = type;
    }

    public TaxException(TaxSubDomain type, TOABError error, Object[] parameters) {
        super(error, type, parameters);
        this.error = error;
        this.type = type;
    }

}
