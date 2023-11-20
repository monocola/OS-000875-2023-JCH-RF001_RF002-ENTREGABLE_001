package pe.gob.servir.entidad.util;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.codec.binary.Base64;

public class EncryptUtil {

//    public static final byte[] KEY = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'};
    public static final byte[] KEY = "OTgyOThkNjktYzlm".getBytes(StandardCharsets.UTF_8);

    private static final String ENCRYPT_ALGO = "AES/GCM/NoPadding";
    private static final int TAG_LENGTH_BIT = 128;
    private static final int IV_LENGTH_BYTE = 12;
    
    public static byte[] getRandomNonce(int numBytes) {
        byte[] nonce = new byte[numBytes];
        new SecureRandom().nextBytes(nonce);
        return nonce;
    }
    
    public static String encrypt(String value, byte[] secretKey) throws Exception {
        byte[] b1 = value.getBytes(StandardCharsets.UTF_8);
        byte[] iv = getRandomNonce(IV_LENGTH_BYTE);

        System.out.println("iv = " + iv);

        SecretKeySpec secret = new SecretKeySpec(secretKey, "AES");
        
        Cipher cipher = Cipher.getInstance(ENCRYPT_ALGO);
        cipher.init(Cipher.ENCRYPT_MODE, secret, new GCMParameterSpec(TAG_LENGTH_BIT, iv));
        byte[] encryptedValue = cipher.doFinal(b1);
        
        byte[] encryptedValueWithIv = ByteBuffer.allocate(iv.length + encryptedValue.length)
                .put(iv)
                .put(encryptedValue)
                .array();

        return Base64.encodeBase64String(encryptedValueWithIv);
    }

    public static String decrypt(String value, byte[] secretKey) throws Exception {
    	
    	byte[] b1 = Base64.decodeBase64(value);

        ByteBuffer bb = ByteBuffer.wrap(b1);

        byte[] iv = new byte[IV_LENGTH_BYTE];
        bb.get(iv);

        System.out.println("iv = " + iv);

        byte[] cipherValue = new byte[bb.remaining()];
        bb.get(cipherValue);
    	
        SecretKeySpec secret = new SecretKeySpec(secretKey, "AES");
        
        Cipher cipher = Cipher.getInstance(ENCRYPT_ALGO);
        cipher.init(Cipher.DECRYPT_MODE, secret, new GCMParameterSpec(TAG_LENGTH_BIT, iv));
        byte[] decryptedValue = cipher.doFinal(cipherValue);

        return new String(decryptedValue, StandardCharsets.UTF_8);
    }
    
    public static void main(String args[]) throws Exception {
        String encryp = EncryptUtil.encrypt("Hola Mundo Ñandú!", KEY);
        System.out.println("ENCRYPTED = " + encryp);
        System.out.println("DECRYPTED = " + EncryptUtil.decrypt(encryp, KEY));
    }
}