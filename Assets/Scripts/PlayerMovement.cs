using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerMovement : MonoBehaviour
{
    Rigidbody rb;

    float yRot = -90f;

    [SerializeField]
    float walkSpeed;
    [SerializeField]
    float turnSpeed;

    void Start()
    {
        rb = GetComponent<Rigidbody>();
    }

    void Update()
    {
        yRot += Input.GetAxis("Mouse X") * turnSpeed;
        if (yRot < 0) yRot += 360f;
        if (yRot > 360f) yRot -= 360f;
        transform.eulerAngles = new Vector3(0, yRot, 0);

        Debug.Log(transform.forward);
    }

    void FixedUpdate()
    {
        Debug.DrawRay(rb.position, transform.forward);
        if (Input.GetKey(KeyCode.W))
        {
            Vector3 currPos = rb.position;
            Vector3 newPos = rb.position + transform.forward * walkSpeed * Time.deltaTime;
            rb.MovePosition(newPos);
        }
    }
}
