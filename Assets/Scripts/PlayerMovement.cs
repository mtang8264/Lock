using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.AI;

public class PlayerMovement : MonoBehaviour
{
    [SerializeField]
    float speed = 2f;

    [SerializeField]
    float turnSpeed;
    [SerializeField]
    float knodSpeed;
    [SerializeField]
    Vector2 knodLimits;
    float knodAngle;

    float navMeshCheckDistance = 1f;

    Rigidbody rb;

    private void Start()
    {
        Cursor.lockState = CursorLockMode.Locked;

        rb = GetComponent<Rigidbody>();
    }

    void Update()
    {
        Turn();
        Knod();
        //NavMove();
    }

    private void FixedUpdate()
    {
        PhysicsMove();
    }

    void Knod()
    {
        knodAngle -= Input.GetAxis("Mouse Y") * knodSpeed * Time.deltaTime;

        if (knodAngle < knodLimits.x) knodAngle = knodLimits.x;
        if (knodAngle > knodLimits.y) knodAngle = knodLimits.y;

        Camera.main.transform.localEulerAngles = new Vector3(knodAngle, 0);
    }

    void Turn()
    {
        if(Mathf.Abs(Input.GetAxis("Mouse X")) > 0.01f)
        {
            transform.Rotate(transform.up, Input.GetAxis("Mouse X") * turnSpeed * Time.deltaTime);
        }
    }

    void PhysicsMove()
    {
        if(Mathf.Abs(Input.GetAxis("Vertical")) > 0.01f)
        {
            Vector3 newPosition = transform.position + transform.forward * Input.GetAxis("Vertical") * speed * Time.deltaTime;

            rb.MovePosition(newPosition);
        }
    }    

    void NavMove()
    {
        if(Mathf.Abs(Input.GetAxis("Vertical")) > 0.01f)
        {
            Vector3 newPosition = transform.position + transform.forward * Input.GetAxis("Vertical") * speed * Time.deltaTime;

            NavMeshHit hit;
            bool isValid = NavMesh.SamplePosition(newPosition, out hit, navMeshCheckDistance, NavMesh.AllAreas);
            if(isValid)
            {
                transform.position = newPosition;
            }
        }
    }
}
