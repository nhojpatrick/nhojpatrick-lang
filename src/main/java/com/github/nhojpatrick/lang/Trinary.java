/*
 * Copyright (c) 2007 - 2010 nhojpatrick.com. All rights reserved.
 */
package com.github.nhojpatrick.lang;

import org.apache.log4j.Logger;

/**
 * The <code>Trinary</code> enum is a strongly typed alternative to; true, false
 * and null.
 * 
 * @author john
 * @todo [IMPL] Trinary String's are lowercase, but valueOf(String) only works
 *       with uppercase by default, i.e. the enum variable name. Determine how
 *       this should be resolved.
 */
public enum Trinary {

    /**
     * Trinary FALSE.
     */
    FALSE('F'),

    /**
     * Trinary TRUE.
     */
    TRUE('T'),

    /**
     * Trinary UNKNOWN.
     */
    UNKNOWN('U');

    /**
     * Returns the Trinary enum constant that represents the supplied pBoolean.
     * Supplying null will return UNKNOWN, Boolean.TRUE will return TRUE and
     * Boolean.FALSE will return FALSE.
     * 
     * @param pBoolean
     *            the Boolean that needs to be resolved to its Trinary enum
     *            representation.
     * @return the Trinary that represents the supplied pBoolean.
     */
    public static Trinary valueOf(final Boolean pBoolean) {

        final Logger LOG = Logger.getLogger(Trinary.class);

        LOG.debug("ENTRY[valueOf=Boolean;Boolean=" + pBoolean + "]");

        final Trinary trinary;
        if (pBoolean == null) {

            trinary = UNKNOWN;

        } else if (pBoolean) {
            trinary = TRUE;

        } else {
            trinary = FALSE;
        }

        LOG.debug("EXIT[valueOf=Boolean;Trinary=" + trinary + "]");

        return trinary;
    }

    /**
     * Returns the Trinary enum constant that represents the supplied pChar. The
     * value 'U' will return UNKNOWN, 'T' will return TRUE and 'F' will return
     * FALSE;
     * 
     * @param pChar
     *            the char that needs to be resolved to its Trinary enum
     *            representation.
     * @return the Trinary that represents the supplied pChar.
     */
    public static Trinary valueOf(final char pChar) {

        final Logger LOG = Logger.getLogger(Trinary.class);

        LOG.debug("ENTRY[valueOf=char;char=" + pChar + "]");

        final Trinary trinary;
        if ('U' == pChar) {
            trinary = UNKNOWN;

        } else if ('T' == pChar) {
            trinary = TRUE;

        } else if ('F' == pChar) {
            trinary = FALSE;

        } else {
            final IllegalArgumentException e = new IllegalArgumentException(
                    "TODO [NHOJ-LANG-000001] No Trinary enum constant for '" + pChar + "'.");
            LOG.error("TODO [NHOJ-LANG-000001]", e);
            throw e;
        }

        LOG.debug("EXIT[valueOf=char;Trinary=" + trinary + "]");

        return trinary;
    }

    private final char trinaryChar;

    /**
     * Constructor Trinary enum with its representations.
     * 
     * @param pTrinaryChar
     *            the Trinary enum's representation as a char.
     */
    private Trinary(final char pTrinaryChar) {

        final Logger LOG = Logger.getLogger(Trinary.class);

        LOG.debug("ENTRY[TrinaryChar=" + pTrinaryChar + "]");

        this.trinaryChar = pTrinaryChar;
    }

    /**
     * Get the Trinary enum as a char.
     * 
     * @return the Trinary enum represented as a char.
     */
    public char getAsChar() {

        return this.trinaryChar;
    }

}
